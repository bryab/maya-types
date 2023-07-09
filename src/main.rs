use core::prelude;
use itertools::Itertools;
use lazy_static::lazy_static;
use rayon::prelude::*;
use regex::Regex;
use scraper::{self, html, ElementRef, Selector};
use std::any::type_name;
use std::error::Error;
use std::fs::{self, File};
use std::io::prelude::Write;
use std::iter;
use std::path::Path;
use std::path::PathBuf;

#[derive(Debug, PartialEq)]
enum FuncMode {
    Create,
    Edit,
    Query,
    Multiuse,
}

/// Convert a MEL type to a Python type, accounting for tuples as well.
fn py_type_from_maya(type_name: &str) -> String {
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
        format!("list[{}]", py_type_from_maya_simple(type_name))
    } else if RE_TUPLE_TYPED.is_match(type_name) {
        let (_, type_name, tuple_length) = RE_TUPLE_TYPED
            .captures(&type_name)
            .unwrap()
            .iter()
            .filter_map(|m| Some(m?.as_str().to_string()))
            .next_tuple()
            .unwrap();

        let type_name = py_type_from_maya_simple(&type_name);
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
            .map(|s| py_type_from_maya_simple(s).to_string())
            .collect();

        format!("Tuple[{}]", type_names.join(", "))
    } else {
        String::from(py_type_from_maya_simple(type_name))
    }
}
/// Just a simple mapping from types found in the Maya docs to native Python types
fn py_type_from_maya_simple(type_name: &str) -> &str {
    if type_name.is_empty() {
        panic!("Type name empty!");
    }
    match type_name.to_lowercase().as_str() {
        "int" | "int64" | "uint" => "int",
        "string" | "node" | "name" | "target" | "script" | "selectionitem" => "Text",
        "boolean" => "bool",
        "float" | "angle" | "double" | "time" | "linear" => "float",
        "floatrange" => "Tuple[float,float]",
        "timerange" => "Tuple[float,float]",
        "any" => "Any",
        _ => {
            println!("Unknown type: {type_name}");
            "Any"
        }
    }
}

/// Some query flags don't specify the return type in a way that is easy to parse.
/// Here I attempt to deduce the type from the name of the parameter
fn py_type_from_flag_name(name: &str) -> Option<&str> {
    let type_name = match name {
        "boundingBox" => "Tuple[float,float,float,float,float,float]",
        _ => return None,
    };
    Some(type_name)
}

/// In order to handle optional flags in functions, we must provide the default value for each flag.
/// Unfortunately this is not known from the documentation.  The standard producedure here in Python
/// Maybe to just make the default `None`, but this does not make sense as these parameters do not accept
/// None as an argument.  As a workaround we must provide a valid default value for each type.
fn default_value_for_type(pytype: &str) -> Option<&str> {
    // FIXME: Need to handle tuples and lists
    let value_str = match pytype {
        "int" => "0",
        "float" => "0.0",
        "Text" => "\"\"",
        "bool" => "False",
        _ => return None,
    };
    Some(value_str)
}
/// FlagDef represents a flag (sortof a parameter) as defined in the Maya documentation
#[derive(Debug)]
struct FlagDef {
    pub longname: String,
    pub shortname: String,
    /// The native MEL/C type, which will need to be converted into Python
    pub type_name: String,
    /// A flag can exist in different 'modes'.  Currently I represent that in this way.
    /// I may change my mind and actually treat flags with multiple modes as multiple flags,
    /// As they operate completely differently in each mode.
    pub modes: Vec<FuncMode>,
    pub description: String,
    // Some flags (I think only in query mode) change the return type of the function
    // This is the most obscure part of the documentation and the hardest item to deduce.
    // pub return_type: Option<String>,
}

/// Parses the main table describing the parameters of the function
fn process_params_table(table: ElementRef) -> Vec<FlagDef> {
    lazy_static! {
        static ref RE_PARAM: Regex = Regex::new(r"(\w+)\((\w+)\)").unwrap();
        static ref SEL_ROW: Selector = Selector::parse("body > table > tbody > tr").unwrap();
        static ref SEL_COL: Selector = Selector::parse("td").unwrap();
        static ref SEL_CODE: Selector = Selector::parse("code").unwrap();
        static ref SEL_IMG: Selector = Selector::parse("img").unwrap();
    }

    table
        .select(&SEL_ROW)
        .skip_while(|row| {
            let cols: Vec<ElementRef> = row.select(&SEL_COL).collect();
            cols.len() != 3
        })
        // Skip the header row
        //.skip(2)
        .tuples()
        .filter_map(|(row1, row2)| {
            // let cols: Vec<ElementRef> = row1.select(&cellselector).collect();
            // if cols.len() != 3 {
            //     println!("Invalid param row: {}", row1.inner_html());
            //     return None;
            // }
            let (param_col, type_col, mode_col) = row1.select(&SEL_COL).next_tuple()?;
            let param_name: String = param_col
                .select(&SEL_CODE)
                .flat_map(|code| code.text())
                .collect();
            // FIXME: make regex static
            let (_, longname, shortname) = RE_PARAM
                .captures(&param_name)?
                .iter()
                .filter_map(|m| Some(m?.as_str().to_string()))
                .next_tuple()?;
            let type_name = type_col
                .select(&SEL_CODE)
                .flat_map(|code| code.text())
                //.map(mel_tuple_type_to_py)
                .collect();
            let modes = mode_col
                .select(&SEL_IMG)
                .filter_map(|img| img.value().attr("title"))
                .map(|mode| match mode {
                    "query" => FuncMode::Query,
                    "edit" => FuncMode::Edit,
                    "create" => FuncMode::Create,
                    "multiuse" => FuncMode::Multiuse,
                    _ => {
                        panic!("Unknown mode: {}", &mode);
                    }
                })
                .collect();
            let description: String = row2
                .select(&SEL_COL)
                .flat_map(|code| code.text())
                .collect::<String>()
                .trim()
                .to_string();
            Some(FlagDef {
                longname,
                shortname,
                type_name,
                description,
                modes,
            })
        })
        //.inspect(|paramdata| println!("{:?}", paramdata))
        .collect()
    // let args: Vec<ArgData> = table.select(&row_selector).filter_map(parse_params_table_row_pair).nth(0).collect();
}

#[derive(Debug)]
struct FunctionDef {
    description: String,
    name: String,
    return_type: Option<String>,
    positional_params: Option<Vec<ParamDef>>,
    flags: Vec<FlagDef>,
}
#[derive(Debug)]
struct ParamDef {
    type_name: String,
    optional: bool,
    variadic: bool,
}

fn parse_synopsis(synopsis: ElementRef) -> Option<Vec<ParamDef>> {
    lazy_static! {
        static ref RE_POSITIONAL_PARAMS: Regex = Regex::new(r"\(([^=]+?)\,").unwrap();
        static ref RE_POSITIONAL_PARAM_ITEMS: Regex = Regex::new(r"(.+?[ $]|\[.+?\])").unwrap();
        static ref RE_OPTIONAL_PARAM: Regex = Regex::new(r"\[(.+)\]").unwrap();
    }
    let synopsis_text = synopsis.text().collect::<String>();
    let positional_param_text = RE_POSITIONAL_PARAMS
        .captures(&synopsis_text)
        .and_then(|c| Some(c.get(1).unwrap().as_str().trim().to_string()))?;

    // Positional params can be separated by spaces, or if they're optional, just by brackets.
    // So like this: string string
    // Or like this: [string][string]

    Some(
        RE_POSITIONAL_PARAM_ITEMS
            .captures_iter(&positional_param_text)
            .map(|c| c.get(1).unwrap().as_str())
            //.inspect(|s| println!("{}", s))
            .map(|s| {
                let (type_name, optional) = if RE_OPTIONAL_PARAM.is_match(&s) {
                    let type_name = RE_OPTIONAL_PARAM
                        .captures(s)
                        .unwrap()
                        .get(1)
                        .unwrap()
                        .as_str();
                    (type_name, true)
                } else {
                    (s, false)
                };

                let (type_name, variadic) = match type_name.ends_with("...") {
                    true => (
                        type_name.split("...").next().unwrap().trim().to_string(),
                        true,
                    ),
                    false => (type_name.trim().to_string(), false),
                };

                ParamDef {
                    type_name,
                    optional,
                    variadic,
                }
            })
            .collect(),
    )
}

fn parse_maya_function_doc<P: AsRef<Path>>(filename: P) -> Result<FunctionDef, Box<dyn Error>> {
    lazy_static! {
        static ref SEL_TABLE: Selector = Selector::parse("a ~ table").unwrap();
        static ref SEL_NAME: Selector = Selector::parse("div#banner td > h1").unwrap();
        static ref SEL_RETURN: Selector = Selector::parse("h2 + table i").unwrap();
        static ref SEL_SYN: Selector = Selector::parse("p#synopsis").unwrap();
        static ref SEL_DESC: Selector = Selector::parse("p#synopsis ~ p").unwrap();
    }

    let html_body: String = fs::read_to_string(filename)?.parse()?;
    let document = scraper::Html::parse_document(&html_body);

    let name: String = document
        .select(&SEL_NAME)
        .next()
        .ok_or("No title found!")?
        .text()
        .next() // NB: Only getting first bit of text, because anything beyond this is not the name of the func.
        .ok_or("No text found in title!")?
        .trim()
        .to_string();

    let positional_params = parse_synopsis(
        document
            .select(&SEL_SYN)
            .next()
            .expect("Should always have synopsis"),
    );

    let description: String = document.select(&SEL_DESC).flat_map(|e| e.text()).collect();

    let return_type: Option<String> = match document.select(&SEL_RETURN).next() {
        Some(e) => Some(e.text().next().unwrap().trim().to_string()),
        None => None,
    };

    // Parse the parameters table
    let tbody = document
        .select(&SEL_TABLE)
        .next()
        .ok_or("No params table found!")?;
    let params = process_params_table(tbody);
    Ok(FunctionDef {
        description,
        name,
        return_type,
        flags: params,
        positional_params,
    })
}

/// Formats a FunctionDef into one or multiple type definitions
fn fmt_func_pys(def: &FunctionDef) -> Vec<String> {
    // This function is a mess and needs a rethink.
    // Just wanted to get the functionality down first
    match &def.positional_params {
        Some(params) => println!("{}: {:?}", def.name, params),
        None => (),
    };

    lazy_static! {
        static ref FLAG_EDIT: FlagDef = FlagDef {
            shortname: String::from("e"),
            longname: String::from("edit"),
            modes: vec![],
            type_name: String::from("boolean"),
            description: String::from("Enable Edit mode"),
        };
        static ref FLAG_QUERY: FlagDef = FlagDef {
            shortname: String::from("q"),
            longname: String::from("query"),
            modes: vec![],
            type_name: String::from("boolean"),
            description: String::from("Enable Query mode"),
        };
    }
    let create_flags: Vec<&FlagDef> = def
        .flags
        .iter()
        .filter(|flag| flag.modes.is_empty() || flag.modes.contains(&FuncMode::Create))
        .collect();
    let mut edit_flags: Vec<&FlagDef> = def
        .flags
        .iter()
        .filter(|flag| flag.modes.contains(&FuncMode::Edit))
        .collect();
    let query_flags: Vec<&FlagDef> = def
        .flags
        .iter()
        .filter(|flag| flag.modes.contains(&FuncMode::Query))
        .collect();

    //let mut num_overloads = 0;

    let mut defs: Vec<String> = vec![];

    if !create_flags.is_empty() {
        let return_type = def
            .return_type
            .clone()
            .map_or(String::from("None"), |s| py_type_from_maya(&s));

        defs.push(fmt_py_def(
            &def.name,
            &fmt_signature(&def.positional_params, &create_flags, FlagNameType::Long),
            &return_type,
            &def.description,
        ));

        defs.push(fmt_py_def(
            &def.name,
            &fmt_signature(&def.positional_params, &create_flags, FlagNameType::Short),
            &return_type,
            &def.description,
        ));
    }

    if !edit_flags.is_empty() {
        let return_type = "None"; // FIXME: Unsure what return type is in edit mode. Same as create maybe?

        edit_flags.insert(0, &FLAG_EDIT);

        defs.push(fmt_py_def(
            &def.name,
            &fmt_signature(&def.positional_params, &edit_flags, FlagNameType::Long),
            &return_type,
            &def.description,
        ));

        defs.push(fmt_py_def(
            &def.name,
            &fmt_signature(&def.positional_params, &edit_flags, FlagNameType::Short),
            &return_type,
            &def.description,
        ));
    }

    if !query_flags.is_empty() {
        // This is where things get a little experimental, and I need to test this in the wild.

        for flag in query_flags {
            // In query mode, the flag's return type is the functions' return type.
            // And the flag is usually (I think?) changed to a boolean.

            // Also, when using query flags in this way it does not make sense to have more than one,
            // So I will do an overload for each flag separately.

            let new_flag = FlagDef {
                longname: flag.longname.clone(),
                shortname: flag.shortname.clone(),
                type_name: String::from("boolean"),
                modes: vec![],
                description: flag.description.clone(),
            };

            let flags: Vec<&FlagDef> = vec![&FLAG_QUERY, &new_flag];

            // This is very hacky - just an experiment.
            // If a query flag is described as a boolean, then it probably
            // does not return a boolean.  We attempt to deduce its return
            // type from its name.  The return type is sometimes described
            // In the description - but that would be hard to parse.
            let return_type: String = match flag.type_name.as_str() {
                "boolean" => String::from(py_type_from_flag_name(&flag.longname).unwrap_or("bool")),
                _ => py_type_from_maya(&flag.type_name),
            };

            defs.push(fmt_py_def(
                &def.name,
                &fmt_signature(&def.positional_params, &flags, FlagNameType::Long),
                &return_type,
                &format!("[Query Mode: {}]\n{}", flag.longname, flag.description),
            ));

            defs.push(fmt_py_def(
                &def.name,
                &fmt_signature(&def.positional_params, &flags, FlagNameType::Short),
                &return_type,
                &format!("[Query Mode: {}]\n{}", flag.longname, flag.description),
            ));
        }
    }

    defs
}

fn fmt_func_py(def: FunctionDef) -> String {
    let defs = fmt_func_pys(&def);
    if defs.is_empty() {
        panic!("No function defs produced for {:?}!", &def)
    } else if defs.len() == 1 {
        defs[0].clone() // FIXME: Surely I can move this String out somehow?
    } else {
        format!("\n@overload\n{0}", defs.join("\n@overload\n"))
    }
}

enum FlagNameType {
    Short,
    Long,
}

fn fmt_signature(
    params: &Option<Vec<ParamDef>>,
    flags: &Vec<&FlagDef>,
    flag_name_type: FlagNameType,
) -> String {
    let mut params = match params {
        Some(params) => params
            .iter()
            .enumerate()
            .map(|(i, param)| {
                format!(
                    "{}{}{}: {}{}",
                    match param.variadic {
                        true => "*",
                        false => "",
                    },
                    &param.type_name,
                    i,
                    py_type_from_maya(&param.type_name),
                    match param.optional && !param.variadic {
                        true => " = None",
                        false => "",
                    }
                )
            })
            .collect(),
        None => vec![],
    };

    params.extend(flags.iter().map(|flag| {
        let mut pytype = py_type_from_maya(&flag.type_name);
        // FIXME: The above func could take the whole flag struct and thus produce this special
        // Multiuse type
        if flag.modes.contains(&FuncMode::Multiuse) {
            pytype = format!("{0} | list[{0}] | Tuple[{0}, ...]", pytype);
        }
        format!(
            "{}: {} = {}",
            match flag_name_type {
                FlagNameType::Long => &flag.longname,
                FlagNameType::Short => &flag.shortname,
            },
            &pytype,
            match default_value_for_type(&pytype) {
                Some(default_value) => default_value,
                None => "None", // FIXME: This is not valid.
            }
        )
    }));
    params.join(", ")
}

fn fmt_py_def(name: &str, signature: &str, return_type: &str, description: &str) -> String {
    format!(
        "def {}({}) -> {}:\n\t\"\"\"\n\t{}\n\t\"\"\"\n\t...",
        name,
        &signature,
        return_type,
        &description.replace("\n", "\n\t").trim()
    )
}

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
        .map(fmt_func_py)
        .collect()
}
fn main() -> std::io::Result<()> {
    let output_filepath = "./typings/maya/cmds/__init__.pyi";
    let mut file = File::create(output_filepath)?;
    writeln!(file, "from typing import Any, Text, Tuple, overload")?;
    for python_def in parse_all_maya_docs("./source_docs/2023/CommandsPython") {
        writeln!(file, "{}", python_def)?;
    }
    Ok(())
}
