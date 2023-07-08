use itertools::Itertools;
use lazy_static::lazy_static;
use rayon::prelude::*;
use regex::Regex;
use scraper::{self, html, ElementRef, Selector};
use std::error::Error;
use std::fs;
use std::iter;
use std::path::Path;
use std::path::PathBuf;
// conversion_table = [
//     [r"float\[3\]",   "Tuple[float, float, float]"],
//     ["string",        "Text"],
//     ["boolean",       "bool"],
//     ["Boolean",       "bool"],
//     ["object(s*)",    "Text"],
//     ["node",          "Text"],
//     ["name",          "Text"],
//     ["target",        "Text"],
//     ["script",        "Text"],
//     ["uint",          "int"],
//     ["angle",         "float"],
//     ["floatrange",    "Tuple[float, float]"],
//     ["timerange",     "Tuple[float, float]"],
//     ["double",        "float"],
//     ["time",          "float"],
//     ["name",          "Text"],
//     ["linear",        "float"],
//     ["STRING",        "Text"],
//     ["Int",           "int"],
//     ["int64",         "int"],
//     ["selectionItem", "Text"],
// ]

#[derive(Debug)]
enum FuncMode {
    Create,
    Edit,
    Query,
}

/// Convert a MEL type to a Python type, accounting for tuples as well.
fn mel_tuple_type_to_py(type_name: &str) -> String {
    lazy_static! {
        static ref RE_TUPLE: Regex = Regex::new(r"(\w+)\[(\d)\]").unwrap();
    }
    let (type_name, tuple_length) = if RE_TUPLE.is_match(type_name) {
        let (_, type_name, tuple_length) = RE_TUPLE
            .captures(&type_name)
            .unwrap()
            .iter()
            .filter_map(|m| Some(m?.as_str().to_string()))
            .next_tuple()
            .unwrap();
        (
            type_name.to_lowercase(),
            tuple_length.parse::<usize>().unwrap(),
        )
    } else {
        return String::from(mel_type_to_py(type_name));
    };

    format!(
        "Tuple[{0}]",
        iter::repeat(type_name).take(tuple_length).join(",")
    )
}
/// Just a simple mapping from types found in the Maya docs to native Python types
fn mel_type_to_py(type_name: &str) -> &str {
    match type_name {
        "int" => "int",
        "string" => "Text",
        "boolean" => "bool",
        "node" => "Text",
        "name" => "Text",
        "target" => "Text",
        "script" => "Text",
        "uint" => "int",
        "angle" => "float",
        "floatrange" => "Tuple[float,float]",
        "timerange" => "Tuple[float,float]",
        "double" => "float",
        "time" => "float",
        "linear" => "float",
        "int64" => "int",
        "selectionitem" => "Text",
        _ => {
            println!("Unknown type: {type_name}");
            type_name
        }
    }

    // [r"float\[3\]",   "Tuple[float, float, float]"],
}
#[derive(Debug)]
struct ParamData {
    pub longname: String,
    pub shortname: String,
    pub type_name: String,
    pub modes: Vec<FuncMode>,
    pub description: String,
}

/// Parses the main table describing the parameters of the function
/// Types are converted into Python native types of typing Types
fn process_params_table(table: ElementRef) -> Vec<ParamData> {
    lazy_static! {
        static ref RE_PARAM: Regex = Regex::new(r"(\w+)\((\w+)\)").unwrap();
        static ref SEL_ROW: Selector = Selector::parse("body > table > tbody > tr").unwrap();
        static ref SEL_COL: Selector = Selector::parse("td").unwrap();
        static ref SEL_CODE: Selector = Selector::parse("code").unwrap();
        static ref SEL_IMG: Selector = Selector::parse("img").unwrap();
    }

    table
        .select(&SEL_ROW)
        // Skip the header row
        .skip(2)
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
                .map(mel_tuple_type_to_py)
                .collect();
            let modes = mode_col
                .select(&SEL_IMG)
                .filter_map(|img| img.value().attr("title"))
                .map(|mode| match mode {
                    "query" => FuncMode::Query,
                    "edit" => FuncMode::Edit,
                    _ => FuncMode::Create,
                })
                .collect();
            let description: String = row2
                .select(&SEL_COL)
                .flat_map(|code| code.text())
                .collect::<String>()
                .trim()
                .to_string();
            Some(ParamData {
                longname,
                shortname,
                type_name,
                description,
                modes,
            })
        })
        .inspect(|paramdata| println!("{:?}", paramdata))
        .collect()
    // let args: Vec<ArgData> = table.select(&row_selector).filter_map(parse_params_table_row_pair).nth(0).collect();
}

#[derive(Debug)]
struct FunctionDef {
    description: String,
    name: String,
    params: Vec<ParamData>,
}

fn process_file<P: AsRef<Path>>(filename: P) -> Result<FunctionDef, Box<dyn Error>> {
    lazy_static! {
        static ref SEL_TABLE: Selector = Selector::parse("a ~ table").unwrap();
    }

    let html_body: String = fs::read_to_string(filename)?.parse()?;
    let document = scraper::Html::parse_document(&html_body);
    let tbody = document
        .select(&SEL_TABLE)
        .next()
        .ok_or("No params table found!")?;
    let params = process_params_table(tbody);
    Ok(FunctionDef {
        description: String::new(),
        name: String::new(),
        params,
    })
}

fn process_files<P: AsRef<Path>>(dirpath: P) -> Vec<FunctionDef> {
    let filenames: Vec<PathBuf> = fs::read_dir(dirpath)
        .unwrap()
        .map(|e| e.unwrap().path())
        .collect();

    filenames
        .into_par_iter()
        .filter_map(|filepath| process_file(filepath).ok())
        .collect()
}
fn main() {
    for def in process_files("./source_docs/2023/CommandsPython") {
        println!("{:?}", def);
    }
}
