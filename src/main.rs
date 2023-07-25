use itertools::Itertools;
use lazy_static::lazy_static;
use once_cell::sync::Lazy;
use rayon::prelude::*;
use regex::Regex;
use scraper::{self, ElementRef, Selector};
use std::cell::RefCell;
use std::collections::HashSet;
use std::error::Error;
use std::fs::{self, File};
use std::io::prelude::Write;
use std::iter;
use std::path::Path;
use std::path::PathBuf;

use clap::{Parser, ValueEnum};

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Source 'CommandsPython' directory containing html files
    #[arg(default_value = "./source_docs/2023/CommandsPython")]
    source: String,
    /// Output .pyi file
    #[arg(default_value = "./output/maya/cmds/__init__.pyi")]
    output: String,
    /// Amount of docstrings generation - Reducing docstrings can help Pylance performance
    #[arg(default_value_t= DocstringLevel::All)]
    #[arg(short, long, value_enum)]
    doclevel: DocstringLevel,
    /// Include short-form flag overloads of all functions.  Only longform syntax is supported by default for performance reasons.
    #[arg(short, long, default_value_t = false)]
    short: bool,
}
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum DocstringLevel {
    /// Do not write any docstrings at all
    None,
    /// Only write docstrings for "create" mode with longform flags
    Minimal,
    /// Write docstrings for all functions with longform flags
    Long,
    /// Write docstrings for all functions
    All,
}
static CONFIG: Lazy<Cli> = Lazy::new(|| Cli::parse());

#[derive(Debug, PartialEq)]
enum FlagMode {
    Create,
    Edit,
    Query,
    Multiuse,
}

/// Convert a MEL type to a Python type, accounting for container types as well.
fn py_type_from_maya(type_name: &str, is_return_type: bool) -> String {
    lazy_static! {
        static ref RE_ARRAY: Regex = Regex::new(r"(\w+)\[\]").unwrap();
        static ref RE_TUPLE_TYPED: Regex = Regex::new(r"(\w+)\[(\d)\]").unwrap();
        static ref RE_TUPLE_MIXED: Regex = Regex::new(r"^\[([a-z, ]+)\]$").unwrap();
    }
    if RE_ARRAY.is_match(&type_name) {
        let type_name = RE_ARRAY
            .captures(&type_name)
            .unwrap()
            .get(1)
            .unwrap()
            .as_str();
        //assert!(!type_name.is_empty());
        format!(
            "list[{}]",
            py_type_from_maya_simple(type_name, is_return_type)
        )
    } else if RE_TUPLE_TYPED.is_match(type_name) {
        let (_, type_name, tuple_length) = RE_TUPLE_TYPED
            .captures(&type_name)
            .unwrap()
            .iter()
            .filter_map(|m| Some(m?.as_str().to_string()))
            .next_tuple()
            .unwrap();
        //assert!(!type_name.is_empty());
        let type_name = py_type_from_maya_simple(&type_name, is_return_type);
        let tuple_length = tuple_length.parse::<usize>().unwrap();

        format!(
            "Tuple[{0}]",
            iter::repeat(type_name).take(tuple_length).join(", ")
        )
    } else if RE_TUPLE_MIXED.is_match(&type_name) {
        let type_names: Vec<String> = RE_TUPLE_MIXED
            .captures(&type_name)
            .unwrap()
            .get(1)
            .unwrap()
            .as_str()
            .split(", ")
            .inspect(|s| {
                if s.is_empty() {
                    panic!("Empty type produced from: {}", type_name);
                }
            })
            .map(|s| py_type_from_maya_simple(s, is_return_type).to_string())
            .collect();

        format!("Tuple[{}]", type_names.join(", "))
    } else {
        String::from(py_type_from_maya_simple(type_name, is_return_type))
    }
}
/// Convert a Maya type to a Python type - only simple non-container types.
fn py_type_from_maya_simple(type_name: &str, is_return_type: bool) -> &str {
    if type_name.is_empty() {
        panic!("Type name empty!");
    }
    match type_name.to_lowercase().as_str() {
        "int" | "int64" | "uint" => "int",
        "boolean" => "bool", // Note that boolean parameters usually accepts 0 or 1 as well, but making this a Union puts unnecessary strain on Pylance
        "float" | "angle" | "double" | "time" | "linear" => "float",
        "floatrange" => "Tuple[float,float]",
        "timerange" => "Tuple[float,float]",
        "any" => "Any",
        "string" | "script" | "name" | "node" | "target" | "selectionitem" | "filename"
        | "message" | "subd" | "stringstring" | "editorname" | "context" | "groupname"
        | "surfaceisoparm" | "imagename" | "attribute" | "contextname" | "panelname" | "curve"
        | "surface" | "poly" | "dagobject" | "camera" | "animatedobject" | "targetlist"
        | "attributelist" | "object" | "objects" | "selectionlist" => {
            // It seems that for flags and parameters, any text parameter accepts a string, a list of strings, or a tuple of strings
            // However, if this is a return type, we don't want to return a union, as this adds ambiguity and makes Pylance very sad.
            if is_return_type {
                "Text"
            } else {
                "TextArg"
            }
        }
        _ => {
            println!("Unknown type: {type_name}");
            "Any"
        }
    }
}

/// In order to handle optional flags in functions, we must provide the default value for each flag.
/// Unfortunately this is not known from the documentation.  The standard procedure here in Python
/// Maybe to just make the default `None`, but this does not make sense as these parameters do not accept
/// None as an argument.  As a workaround we must provide a valid default value for each type.
fn default_value_for_type(pytype: &str) -> Option<&str> {
    // FIXME: Need to handle containers
    // To do so may require a retink of how the 'pytypes' are stored as it seems
    // Silly to parse a string here, as it's already been done upstream.
    let value_str = match pytype {
        "int" => "0",
        "float" => "0.0",
        "Text" | "TextArg" => "\"\"",
        "bool" => "False",
        _ => return None,
    };
    Some(value_str)
}
/// FlagDef represents a flag (sortof a kwarg) as defined in the Maya documentation
#[derive(Debug)]
struct MayaFlagDef {
    pub longname: String,
    pub shortname: String,
    /// The native MEL/C type, which will need to be converted into Python
    pub type_name: String,
    /// A flag can exist in different 'modes'.  Currently I represent that in this way.
    /// I may change my mind and actually treat flags with multiple modes as multiple flags,
    /// As they operate completely differently in each mode.
    pub modes: Vec<FlagMode>,
    pub description: String,
    /// This is used to label the main switch flags (query, edit) so that they are required in their overloads.
    pub required: bool,
}

/// ParamDef describes a positional parameter as opposed to a flag.
#[derive(Debug)]
struct MayaParamDef {
    /// The native MEL/C type, which will need to be converted into Python
    type_name: String,
    mode: ParamMode,
}
#[derive(Debug, PartialEq)]
enum ParamMode {
    Default,
    Optional,
    Variadic,
}
#[derive(Debug, PartialEq)]
struct ReturnType {
    name: String,
    description: String,
}

#[derive(Debug)]
/// FunctionDef contains all the useful data parsed from a function in the Maya documentation
struct MayaFuncDef {
    description: String,
    name: String,
    return_type: Vec<ReturnType>,
    params: Vec<MayaParamDef>,
    flags: Vec<MayaFlagDef>,
    /// The modes that this function supports, such as "query".
    /// Note that even if there are no flags for this function with a given mode, if the mode is set here,
    /// Then the function should work in that mode with no flags.
    modes: Vec<FlagMode>,
}

impl MayaFuncDef {
    pub fn new(name: &str) -> Self {
        MayaFuncDef {
            description: String::new(),
            name: name.to_string(),
            return_type: vec![],
            params: vec![],
            flags: vec![],
            modes: vec![],
        }
    }

    pub fn add_param(&mut self, type_name: &str, mode: ParamMode) {
        self.params.push(MayaParamDef {
            type_name: type_name.to_string(),
            mode,
        })
    }

    pub fn add_flag(
        &mut self,
        longname: &str,
        shortname: &str,
        type_name: &str,
        modes: Vec<FlagMode>,
    ) {
        self.flags.push(MayaFlagDef {
            longname: longname.to_string(),
            shortname: shortname.to_string(),
            type_name: type_name.to_string(),
            modes,
            description: String::new(),
            required: false,
        })
    }

    pub fn set_modes(&mut self, modes: Vec<FlagMode>) {
        self.modes = modes;
    }

    pub fn set_return_type(&mut self, return_type: &str) {
        self.return_type = vec![ReturnType {
            name: return_type.to_string(),
            description: String::new(),
        }];
    }
}

/// Parses the main table describing the parameters of the function
fn process_flags_table(table: ElementRef) -> Vec<MayaFlagDef> {
    lazy_static! {
        static ref RE_PARAM: Regex = Regex::new(r"(\w+)\((\w+)\)").unwrap();
        static ref SEL_ROW: Selector = Selector::parse("body > table > tbody > tr").unwrap();
        static ref SEL_COL: Selector = Selector::parse("td").unwrap();
        static ref SEL_DESC: Selector =
            Selector::parse("td > table > tbody > tr > td + td").unwrap();
        static ref SEL_CODE: Selector = Selector::parse("code").unwrap();
        static ref SEL_IMG: Selector = Selector::parse("img").unwrap();
    }

    table
        .select(&SEL_ROW)
        .skip_while(|row| {
            let cols: Vec<ElementRef> = row.select(&SEL_COL).collect();
            cols.len() != 3
        })
        .tuples()
        .filter_map(|(row1, row2)| {
            let (param_col, type_col, mode_col) = row1.select(&SEL_COL).next_tuple()?;
            let param_name: String = param_col
                .select(&SEL_CODE)
                .flat_map(|code| code.text())
                .collect();
            let (_, longname, shortname) = RE_PARAM
                .captures(&param_name)?
                .iter()
                .filter_map(|m| Some(m?.as_str().to_string()))
                .next_tuple()?;
            let type_name = type_col
                .select(&SEL_CODE)
                .flat_map(|code| code.text())
                .collect();
            let modes = mode_col
                .select(&SEL_IMG)
                .filter_map(|img| img.value().attr("title"))
                .map(|mode| match mode {
                    "query" => FlagMode::Query,
                    "edit" => FlagMode::Edit,
                    "create" => FlagMode::Create,
                    "multiuse" => FlagMode::Multiuse,
                    _ => {
                        panic!("Unknown mode: {}", &mode);
                    }
                })
                .collect();
            let description: String = row2
                .select(&SEL_DESC)
                .flat_map(|code| code.text())
                .map(|text| text.trim().replace("\n", " "))
                .collect::<String>();

            Some(MayaFlagDef {
                longname,
                shortname,
                type_name,
                description,
                modes,
                required: false,
            })
        })
        .collect()
}

/// Parse the 'synopsis' div to get information about the positional parameters.
/// The flags are ignored as they are described better in the table below it.
fn parse_synopsis(synopsis: ElementRef) -> Option<Vec<MayaParamDef>> {
    lazy_static! {
        static ref RE_POSITIONAL_PARAMS: Regex = Regex::new(r"\(([^=]+?)[,\)]").unwrap();
        static ref RE_POSITIONAL_PARAM_ITEMS: Regex = Regex::new(r"[a-z,A-Z]+ ?\.?+").unwrap();
    }
    let synopsis_text = synopsis.text().collect::<String>();
    let positional_param_text = RE_POSITIONAL_PARAMS
        .captures(&synopsis_text)
        .and_then(|c| Some(c.get(1).unwrap().as_str().trim().to_string()))?;

    // The syntax of the positional parameters is a bit odd, so I am parsing it in an odd way.
    // I just find where the 'optional' begins first (first bracket)
    // Then iterate over all the words and check if they're beyond the optional index.

    let optional_index = positional_param_text.find("[");

    Some(
        RE_POSITIONAL_PARAM_ITEMS
            .captures_iter(&positional_param_text)
            .map(|c| {
                let cap = c.get(0).unwrap();
                let optional = match optional_index {
                    Some(optional_index) => cap.start() > optional_index,
                    None => false,
                };
                let type_name = cap.as_str();
                let (type_name, variadic) = match type_name.ends_with("...") {
                    true => (
                        type_name.split("...").next().unwrap().trim().to_string(),
                        true,
                    ),
                    false => (type_name.trim().to_string(), false),
                };

                let mode = if variadic {
                    ParamMode::Variadic
                } else if optional {
                    ParamMode::Optional
                } else {
                    ParamMode::Default
                };

                MayaParamDef { type_name, mode }
            })
            //.inspect(|s| println!("{:?}", s))
            .collect(),
    )
}

fn fmt_func_py(def: MayaFuncDef) -> String {
    let defs = fmt_func_pys(&def);
    if defs.is_empty() {
        panic!("No function defs produced for {:?}!", &def)
    } else if defs.len() == 1 {
        defs.into_iter().next().unwrap()
    } else {
        format!("@overload\n{0}", defs.join("\n@overload\n"))
    }
}

enum FlagNameType {
    Short,
    Long,
}

fn fmt_signature(params: &Vec<PyParamDef>) -> String {
    if params.is_empty() {
        return "".to_string();
    }
    // There are cases where there's a variadic parameter before others
    // In the maya docs.  We ignore all params that follow the variadic.
    let mut variadic_broken = false;
    params
        .iter()
        .filter(|param| {
            if param.variadic {
                if variadic_broken {
                    return false;
                }
                variadic_broken = true;
            }
            return true;
        })
        .map(|param| {
            format!(
                "{}{}: {}{}",
                match param.variadic {
                    true => "*",
                    false => "",
                },
                param.name,
                param.type_name,
                match &param.default_value {
                    Some(default_value) => format!(" = {}", &default_value),
                    None => String::new(),
                }
            )
        })
        .join(", ")
}

fn fmt_py_def(name: &str, params: &Vec<PyParamDef>, type_name: &str, description: &str) -> String {
    let signature = fmt_signature(params);

    if description.is_empty() {
        format!("def {}({}) -> {}: ...", name, &signature, type_name)
    } else {
        let param_docstring: String = params
            .iter()
            .map(|param| {
                format!(
                    "\n:param {}: {}",
                    param.name,
                    param.description.replace("\n", "; ")
                )
            })
            .collect();

        let description = format!("{}{}", description, param_docstring);

        format!(
            "def {}({}) -> {}:\n\t\"\"\"\n\t{}\n\t\"\"\"\n\t...",
            name,
            &signature,
            type_name,
            description.replace("\n", "\n\t").trim()
        )
    }
}

struct PyParamDef {
    name: String,
    type_name: String,
    default_value: Option<String>,
    description: String,
    variadic: bool,
}

fn py_params_from_maya(
    params: &Vec<MayaParamDef>,
    flags: &Vec<&MayaFlagDef>,
    name_type: FlagNameType,
) -> Vec<PyParamDef> {
    let param_names = RefCell::new(HashSet::new());

    let kwargs: Vec<PyParamDef> = flags
        .iter()
        .map(|flag| {
            assert!(!&flag.type_name.is_empty());
            let type_name = py_type_from_maya(&flag.type_name, false);
            let (type_name, default_value): (String, Option<String>) = if flag.required {
                (type_name, None)
            } else {
                match default_value_for_type(&type_name) {
                    Some(default_value) => (type_name.clone(), Some(default_value.to_string())), // FIXME: Clone should be unnecessary here.
                    None => (
                        format!("Optional[{}]", &type_name),
                        Some("None".to_string()),
                    ),
                }
            };
            let mut name = match name_type {
                FlagNameType::Long => &flag.longname,
                FlagNameType::Short => &flag.shortname,
            }
            .clone();
            while param_names.borrow().contains(&name) {
                println!(
                    "Error: Repeat kwarg name, should not be possible: {}",
                    &name
                );
                name = format!("{}_", name);
            }
            param_names.borrow_mut().insert(name.clone());

            // NB: If type is already 'TextArg', then that is handling the multi-use flag.
            let type_name = if &type_name != "TextArg" && flag.modes.contains(&FlagMode::Multiuse) {
                format!("{0} | list[{0}] | Tuple[{0},...]", &type_name)
            } else {
                type_name.to_string()
            };
            PyParamDef {
                name: name.to_string(),
                type_name,
                default_value: default_value,
                description: flag.description.clone(),
                variadic: false,
            }
        })
        .collect(); // Note I am collecting this because I need kwargs to be processed first.

    let mut args: Vec<PyParamDef> = params
        .iter()
        .map(|param| {
            let type_name = py_type_from_maya(&param.type_name, false);
            let (type_name, default_value) = if param.mode == ParamMode::Optional {
                match default_value_for_type(&type_name) {
                    Some(default_value) => (type_name.clone(), Some(default_value.to_string())), // FIXME: Clone should be unnecessary here.
                    None => (
                        format!("Optional[{}]", &type_name),
                        Some("None".to_string()),
                    ),
                }
            } else {
                (type_name, None)
            };

            let mut name = param.type_name.clone();
            while param_names.borrow().contains(&name) {
                name = format!("{}_", name);
            }
            param_names.borrow_mut().insert(name.clone());

            PyParamDef {
                name,
                type_name,
                default_value,
                description: String::new(), // There's no description for positional parameters in the docs.
                variadic: param.mode == ParamMode::Variadic,
            }
        })
        .collect();

    // Quick fix - If there are no args, then make an "Any" variadic.
    // There are many functions that take arguments, but they are totally undocumented except in the examples.

    if args.len() == 0 {
        args.push(PyParamDef {
            name: "unknown".to_string(),
            type_name: "Any".to_string(),
            default_value: None,
            description: "Unknown".to_string(),
            variadic: true,
        })
    } else {
        // Quick fix - If the final arg is not variadic, make it so.
        // Most of the time, if a function takes a list, it also takes it as multiple arguments instead.
        let final_arg = args.last_mut().unwrap();
        if !final_arg.variadic {
            final_arg.variadic = true;
            final_arg.default_value = None;
        }
    }

    args.extend(kwargs);
    args
}

fn fmt_py_long_and_short(
    name: &str,
    params: &Vec<MayaParamDef>,
    flags: &Vec<&MayaFlagDef>,
    return_type: &str,
    description: &str,
) -> Vec<String> {
    let desclevel = CONFIG.doclevel;
    let do_short = CONFIG.short;
    let mut defs = vec![fmt_py_def(
        &name,
        &py_params_from_maya(&params, &flags, FlagNameType::Long),
        &return_type,
        match desclevel {
            DocstringLevel::None => "",
            _ => &description,
        },
    )];

    if do_short {
        defs.push(fmt_py_def(
            &name,
            &py_params_from_maya(&params, &flags, FlagNameType::Short),
            &return_type,
            match desclevel {
                DocstringLevel::All => &description,
                _ => "",
            },
        ));
    }
    defs
}

// There is an ambiguity in functions that have multiple return types.
// Usually, there is one return type when not in query mode, and the query mode return types have the word "query" in the description
fn fmt_return_type(return_types: &Vec<ReturnType>) -> String {
    if return_types.is_empty() {
        String::from("None")
    } else {
        return_types
            .iter()
            .map(|t| py_type_from_maya(&t.name, true))
            .unique()
            .join(" | ")
    }
}
/// Formats a FunctionDef into one or multiple type definitions
fn fmt_func_pys(def: &MayaFuncDef) -> Vec<String> {
    lazy_static! {
        static ref FLAG_EDIT: MayaFlagDef = MayaFlagDef {
            shortname: String::from("e"),
            longname: String::from("edit"),
            modes: vec![],
            type_name: String::from("boolean"),
            description: String::from("Enable Edit mode"),
            required: true
        };
        static ref FLAG_QUERY: MayaFlagDef = MayaFlagDef {
            shortname: String::from("q"),
            longname: String::from("query"),
            modes: vec![],
            type_name: String::from("boolean"),
            description: String::from("Enable Query mode"),
            required: true
        };
    }

    let desclevel = CONFIG.doclevel;

    let create_flags: Vec<&MayaFlagDef> = def
        .flags
        .iter()
        .filter(|flag| {
            flag.modes.is_empty()
                || flag.modes.len() == 1 && *flag.modes.first().unwrap() == FlagMode::Multiuse
                || flag.modes.contains(&FlagMode::Create)
        })
        .collect();

    let mut defs: Vec<String> = vec![];

    //let return_types = &def.return_type;

    // if return_types.len() > 1 {
    //     println!("{}", def.name);
    // }

    if !create_flags.is_empty() {
        let return_type = fmt_return_type(&def.return_type);

        defs.extend(
            fmt_py_long_and_short(
                &def.name,
                &def.params,
                &create_flags,
                &return_type,
                &def.description,
            )
            .into_iter(),
        );
    }

    if def.modes.contains(&FlagMode::Edit) {
        let mut edit_flags: Vec<&MayaFlagDef> = def
            .flags
            .iter()
            .filter(|flag| flag.modes.contains(&FlagMode::Edit))
            .collect();

        let return_type = "None"; // FIXME: Unsure what return type is in edit mode. Same as create maybe?

        edit_flags.insert(0, &FLAG_EDIT);

        defs.extend(
            fmt_py_long_and_short(
                &def.name,
                &def.params,
                &edit_flags,
                &return_type,
                match desclevel {
                    DocstringLevel::All => &def.description,
                    _ => "",
                },
            )
            .into_iter(),
        );
    }

    if def.modes.contains(&FlagMode::Query) {
        //let mut num_overloads = 0;

        // This is where things get a little experimental, and I need to test this in the wild.

        // In query mode, many of the flags change the return type of the function, but not all.
        // Some are documented as the type of the flag, some are just 'boolean', and some vary on other conditions.
        // For now, we only handle overloading the query flags that actually specify a type.
        // For all other query flags, we lump them into a single overload which returns Any.

        let (query_flags, query_switch_flags): (Vec<&MayaFlagDef>, Vec<&MayaFlagDef>) = def
            .flags
            .iter()
            .filter(|flag| flag.modes.contains(&FlagMode::Query))
            .partition(|flag| flag.type_name == "boolean");

        let return_type = "Any";

        let mut flags: Vec<&MayaFlagDef> = vec![&FLAG_QUERY];
        flags.extend(&query_flags);

        defs.extend(fmt_py_long_and_short(
            &def.name,
            &def.params,
            &flags,
            &return_type,
            match desclevel {
                DocstringLevel::All | DocstringLevel::Long => &def.description,
                _ => "",
            },
        ));

        for flag in query_switch_flags {
            let new_flag = MayaFlagDef {
                longname: flag.longname.clone(),
                shortname: flag.shortname.clone(),
                type_name: String::from("boolean"),
                modes: vec![],
                description: match desclevel {
                    DocstringLevel::All | DocstringLevel::Long => flag.description.clone(),
                    _ => String::new(),
                },
                required: true,
            };

            // The flags for this will be a required flag for the 'query switch', plus all the other unknown query flags
            let mut flags: Vec<&MayaFlagDef> = vec![&FLAG_QUERY, &new_flag];
            flags.extend(&query_flags);

            let return_type = py_type_from_maya(&flag.type_name, true);

            defs.extend(
                fmt_py_long_and_short(
                    &def.name,
                    &def.params,
                    &flags,
                    &return_type,
                    match desclevel {
                        DocstringLevel::All | DocstringLevel::Long => &flag.description,
                        _ => "",
                    },
                )
                .into_iter(),
            );
        }
    }

    if defs.is_empty() {
        let return_type = fmt_return_type(&def.return_type);

        defs.push(fmt_py_def(
            &def.name,
            &py_params_from_maya(&def.params, &vec![], FlagNameType::Long),
            &return_type,
            match desclevel {
                DocstringLevel::None => "",
                _ => &def.description,
            },
        ));
    }

    defs
}

fn parse_maya_function_doc<P: AsRef<Path>>(filename: P) -> Result<MayaFuncDef, Box<dyn Error>> {
    lazy_static! {
        static ref SEL_TABLE: Selector = Selector::parse("a ~ table").unwrap();
        static ref SEL_NAME: Selector = Selector::parse("div#banner td > h1").unwrap();
        static ref SEL_RETURN: Selector = Selector::parse("h2 + table tr").unwrap();
        static ref SEL_RETURN_NAME: Selector = Selector::parse("td > i").unwrap();
        static ref SEL_RETURN_DESC: Selector = Selector::parse("td + td").unwrap();
        static ref SEL_SYN: Selector = Selector::parse("p#synopsis").unwrap();
        static ref SEL_DESC: Selector = Selector::parse("p#synopsis + p ~ p").unwrap();
        static ref SEL_MODES_DESC: Selector = Selector::parse("p#synopsis + p").unwrap();
        static ref RE_TITLE: Regex = Regex::new(r"[a-z,A-Z]+").unwrap();
        static ref RE_MODES: Regex = Regex::new(r"[^T] (queryable|editable)").unwrap();
        static ref RE_EXTRA_WHITESPACE: Regex = Regex::new(r"\n[\n\t ]+").unwrap();
    }

    let html_body: String = fs::read_to_string(filename)?.parse()?;
    let document = scraper::Html::parse_document(&html_body);

    let name = document
        .select(&SEL_NAME)
        .next()
        .ok_or("No title found!")?
        .text()
        .next() // NB: Only getting first bit of text, because anything beyond this is not the name of the func.
        .ok_or("No text found in title!")?
        .trim();

    let name = RE_TITLE.find(&name).unwrap().as_str().to_string();

    let params = parse_synopsis(
        document
            .select(&SEL_SYN)
            .next()
            .expect("Should always have synopsis"),
    )
    .unwrap_or(vec![]);

    //assert!(positional_params.len() > 0);

    let description = document
        .select(&SEL_DESC)
        .flat_map(|e| e.text())
        .collect::<String>();

    let description = RE_EXTRA_WHITESPACE
        .replace_all(&description.trim(), "\n")
        .to_string();

    let modes_description = document
        .select(&SEL_MODES_DESC)
        .flat_map(|e| e.text())
        .collect::<String>();

    let modes: Vec<FlagMode> = RE_MODES
        .captures_iter(&modes_description)
        .filter_map(|c| c.get(1))
        .filter_map(|m| match m.as_str() {
            "queryable" => Some(FlagMode::Query),
            "editable" => Some(FlagMode::Edit),
            _ => None,
        })
        .collect();

    let return_type = document
        .select(&SEL_RETURN)
        .filter_map(|tr| {
            Some(ReturnType {
                name: tr
                    .select(&SEL_RETURN_NAME)
                    .next()?
                    .text()
                    .next()?
                    .trim()
                    .to_string(),
                description: tr
                    .select(&SEL_RETURN_DESC)
                    .next()?
                    .text()
                    .next()?
                    .trim()
                    .to_string(),
            })
        })
        .collect();

    let flags = match document.select(&SEL_TABLE).next() {
        Some(table) => process_flags_table(table),
        None => vec![],
    };

    for param in &params {
        assert!(
            !&param.type_name.is_empty(),
            "{}: Empty param from {:?}",
            &name,
            &params
        );
    }

    Ok(MayaFuncDef {
        description,
        name,
        return_type,
        flags,
        params,
        modes,
    })
}

fn apply_func_fixes(mut def: MayaFuncDef) -> MayaFuncDef {
    // This is just a test of a possible pipeline, which would load
    // Fixes to the documentation from an external file.
    if def.name == "file" {
        // In the 'file' documentation, many flags are listed as only available in 'query', but they are available in 'create' too.
        def.params = vec![MayaParamDef {
            type_name: "filename".to_string(),
            mode: ParamMode::Default,
        }];

        let flags_to_fix = ["namespace", "reference"];

        let flags: Vec<&mut MayaFlagDef> = def
            .flags
            .iter_mut()
            .filter(|flag| flags_to_fix.contains(&flag.longname.as_str()))
            .collect();
        for flag in flags {
            flag.modes = vec![FlagMode::Create, FlagMode::Query];
        }
    }
    def
}

fn add_missing_funcs() -> Vec<MayaFuncDef> {
    // This is just a test of a possible pipeline, which would load
    // Fixes to the documentation from an external file.

    let mut defs: Vec<MayaFuncDef> = vec![];
    let mut f = MayaFuncDef::new("FBXExport");
    f.add_param("string", ParamMode::Default);
    f.set_return_type("any");
    defs.push(f);

    let mut f = MayaFuncDef::new("AbcExport");
    f.add_param("string", ParamMode::Default);
    f.add_flag("jobArg", "j", "string", vec![FlagMode::Multiuse]);
    f.add_flag("preRollStartFrame", "prs", "double", vec![]);
    f.add_flag("dontSkipUnwrittenFrames", "duf", "boolean", vec![]);
    f.set_return_type("any");
    defs.push(f);

    let mut f = MayaFuncDef::new("houdiniAsset");
    f.add_param("string", ParamMode::Default);
    f.add_flag("cookMessages", "cm", "string", vec![]);
    f.add_flag("loadAsset", "la", "string", vec![]);
    f.add_flag("listAssets", "ls", "string", vec![]);
    f.add_flag("reloadAsset", "rl", "string", vec![]);
    f.add_flag("resetSimulation", "rs", "string", vec![]);
    f.add_flag("syncAttributes", "sa", "bool", vec![]);
    f.add_flag("syncHidden", "shi", "boolean", vec![]);
    f.add_flag("syncOutputs", "so", "boolean", vec![]);
    f.add_flag("syncTemplatedGeos", "stm", "boolean", vec![]);
    f.add_flag("syncName", "syn", "boolean", vec![]);
    f.set_return_type("string");
    defs.push(f);

    let mut f = MayaFuncDef::new("invertShape");
    f.add_param("string", ParamMode::Default);
    f.add_param("string", ParamMode::Default);
    f.set_return_type("string");
    defs.push(f);

    defs
}
/// Parses all the files in a Maya documentation folder, producing a Vec of the Python defintion of each
fn parse_all_maya_docs<P: AsRef<Path>>(dirpath: P) -> Vec<String> {
    let filenames: Vec<PathBuf> = fs::read_dir(dirpath)
        .unwrap()
        .filter_map(|e| {
            let path = e.unwrap().path();
            match path.extension().unwrap().to_str().unwrap() {
                "html" => Some(path),
                _ => None,
            }
        })
        .collect();

    filenames
        .into_par_iter()
        .filter_map(|filepath| parse_maya_function_doc(filepath).ok())
        .map(apply_func_fixes)
        .chain(add_missing_funcs())
        .map(fmt_func_py)
        .collect()
}

fn main() -> std::io::Result<()> {
    let output_filepath = Path::new(&CONFIG.output);
    let output_parent = output_filepath.parent().expect("Invalid path");
    if !output_parent.exists() {
        fs::create_dir_all(output_parent)?;
    }
    let mut file = File::create(&output_filepath)?;
    writeln!(
        file,
        "from typing import Any, Text, Tuple, overload, Optional, Literal\nTextArg = Text | list[Text] | Tuple[Text, ...]"
    )?;
    for python_def in parse_all_maya_docs(&CONFIG.source) {
        writeln!(file, "{}", python_def)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    #[test]
    fn test_xform() {
        let filepath = "./source_docs/2023/CommandsPython/xform.html";
        let result = parse_maya_function_doc(&filepath).unwrap();
        assert_eq!(result.name, "xform");
        assert_eq!(result.flags.len(), 33);
        assert_eq!(result.params.len(), 1);
        assert_eq!(result.params[0].type_name, "objects");
        assert_eq!(result.params[0].mode, ParamMode::Variadic);
        fmt_func_pys(&result);
        // assert_eq!(s.length() > 0);
    }

    #[test]
    fn test_delete() {
        let filepath = "./source_docs/2023/CommandsPython/delete.html";
        let result = parse_maya_function_doc(&filepath).unwrap();
        assert_eq!(result.name, "delete");
        assert_eq!(result.flags.len(), 14);
        assert_eq!(result.params.len(), 1);
        assert_eq!(result.params[0].type_name, "objects");
        assert_eq!(result.params[0].mode, ParamMode::Default);
        fmt_func_pys(&result);
        // assert_eq!(s.length() > 0);
    }
    #[allow(non_snake_case)]
    #[test]
    fn test_bakeSimulation() {
        let filepath = "./source_docs/2023/CommandsPython/bakeSimulation.html";
        let result = parse_maya_function_doc(&filepath).unwrap();
        assert_eq!(result.name, "bakeSimulation");
        assert_eq!(result.flags.len(), 21);
        assert_eq!(result.params.len(), 1);
        assert_eq!(result.params[0].type_name, "objects");
        assert_eq!(result.params[0].mode, ParamMode::Default);
        fmt_func_pys(&result);
        //assert_eq!(s.length() > 0);
    }
    #[allow(non_snake_case)]
    #[test]
    fn test_poleVectorConstraint() {
        let filepath = "./source_docs/2023/CommandsPython/poleVectorConstraint.html";
        let result = parse_maya_function_doc(&filepath).unwrap();
        assert_eq!(result.name, "poleVectorConstraint");
        assert_eq!(result.flags.len(), 6);
        assert_eq!(result.params.len(), 2);
        assert_eq!(result.params[0].type_name, "target");
        assert_eq!(result.params[0].mode, ParamMode::Variadic);
        assert_eq!(result.params[1].type_name, "object");
        assert_eq!(result.params[1].mode, ParamMode::Optional);
        fmt_func_pys(&result);
        //assert_eq!(s.length() > 0);
    }

    #[allow(non_snake_case)]
    #[test]
    fn test_bevelPlus() {
        let filepath = "./source_docs/2023/CommandsPython/bevelPlus.html";
        let result = parse_maya_function_doc(&filepath).unwrap();
        assert_eq!(result.name, "bevelPlus");
        assert_eq!(result.flags.len(), 11);
        //assert_eq!(result.params.len(), 3);
        assert_eq!(result.params[0].type_name, "curve");
        assert_eq!(result.params[0].mode, ParamMode::Default);
        assert_eq!(result.params[1].type_name, "curve");
        assert_eq!(result.params[1].mode, ParamMode::Optional);
        // assert_eq!(result.params[1].type_name, "curve__");
        // assert_eq!(result.params[1].variadic, true);
        // assert_eq!(result.params[1].optional, true);
        fmt_func_pys(&result);
        //assert_eq!(s.length() > 0);
    }

    #[allow(non_snake_case)]
    #[test]
    fn test_objExists() {
        let filepath = "./source_docs/2023/CommandsPython/objExists.html";
        let result = parse_maya_function_doc(&filepath).unwrap();
        assert_eq!(result.name, "objExists");
        assert_eq!(result.flags.len(), 0);
        assert_eq!(result.params[0].type_name, "string");
        assert_eq!(result.params[0].mode, ParamMode::Default);
        fmt_func_pys(&result);
    }

    #[allow(non_snake_case)]
    #[test]
    fn test_eval() {
        let filepath = "./source_docs/2023/CommandsPython/eval.html";
        let result = parse_maya_function_doc(&filepath).unwrap();
        assert_eq!(result.name, "eval");
        assert_eq!(result.flags.len(), 0);
        assert_eq!(result.params[0].type_name, "string");
        assert_eq!(result.params[0].mode, ParamMode::Default);
        fmt_func_pys(&result);
    }

    #[allow(non_snake_case)]
    #[test]
    fn test_group() {
        let filepath = "./source_docs/2023/CommandsPython/group.html";
        let result = parse_maya_function_doc(&filepath).unwrap();
        assert_eq!(result.name, "group");
        assert!(!result.modes.contains(&FlagMode::Query));
        assert_eq!(result.description, "If the -em flag is specified, then an empty group (with no\nobjects) is created.\nIf the -w flag is specified then the new group is placed under the\nworld, otherwise if -p is specified it is placed under the\nspecified node. If neither -w or -p is specified the new group is\nplaced under the lowest common group they have in common. (or the\nworld if no such group exists)\nIf an object is grouped with another object that has the same name\nthen one of the objects will be renamed by this command.");
        assert_eq!(result.flags.len(), 7);
        assert_eq!(result.params[0].type_name, "objects");
        assert_eq!(result.params[0].mode, ParamMode::Variadic);
        assert_eq!(result.flags[0].longname, "absolute");
        assert_eq!(result.flags[0].description, "preserve existing world object transformations (overall object transformation is preserved by modifying the objects local transformation) [default]");
        fmt_func_pys(&result);
    }

    #[allow(non_snake_case)]
    #[test]
    fn test_currentTime() {
        let filepath = "./source_docs/2023/CommandsPython/currentTime.html";
        let result = parse_maya_function_doc(&filepath).unwrap();
        assert_eq!(result.name, "currentTime");
        assert!(result.modes.contains(&FlagMode::Query));
        fmt_func_pys(&result);
    }

    #[allow(non_snake_case)]
    #[test]
    fn test_connectionInfo() {
        let filepath = "./source_docs/2023/CommandsPython/connectionInfo.html";
        let result = parse_maya_function_doc(&filepath).unwrap();
        assert_eq!(result.name, "connectionInfo");
        assert_eq!(result.return_type[0].name, "boolean");
        assert_eq!(
            result.return_type[0].description,
            "When asking for a property, depending on the flags used."
        );
        assert_eq!(result.return_type[1].name, "string");
        assert_eq!(
            result.return_type[1].description,
            "When asking for a plug name."
        );
        fmt_func_pys(&result);
    }

    #[allow(non_snake_case)]
    #[test]
    fn test_file() {
        let filepath = "./source_docs/2023/CommandsPython/file.html";
        let result = parse_maya_function_doc(&filepath).unwrap();
        let result = apply_func_fixes(result);
        assert_eq!(result.name, "file");
        let flag = result
            .flags
            .iter()
            .find(|flag| flag.longname == "reference")
            .unwrap();
        assert_eq!(flag.modes, vec![FlagMode::Create, FlagMode::Query]);
        assert_eq!(flag.type_name, "boolean");
        let flag = result
            .flags
            .iter()
            .find(|flag| flag.longname == "namespace")
            .unwrap();
        assert_eq!(flag.modes, vec![FlagMode::Create, FlagMode::Query]);
        assert_eq!(flag.type_name, "string");
        fmt_func_pys(&result);
    }
}
