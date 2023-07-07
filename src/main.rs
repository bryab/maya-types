use itertools::Itertools;
use regex;
use scraper::{self, html, ElementRef, Selector};
use std::error::Error;
use std::fs;

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
fn convert_type_name(type_name: &str) -> &str {
    match type_name {
        "string" => "Text",
        _ => type_name,
    }
}
#[derive(Debug)]
struct ParamData {
    pub longname: String,
    pub shortname: String,
    pub type_name: String,
    pub modes: Vec<FuncMode>,
    pub description: String,
}

fn process_params_table(table: ElementRef) -> Vec<ParamData> {
    let row_selector = Selector::parse("body > table > tbody > tr").unwrap();
    let cellselector = Selector::parse("td").unwrap();
    let codeselector = Selector::parse("code").unwrap();
    let imgselector = Selector::parse("img").unwrap();
    table
        .select(&row_selector)
        // Skip the header row
        .skip(2)
        .tuples()
        .filter_map(|(row1, row2)| {
            // let cols: Vec<ElementRef> = row1.select(&cellselector).collect();
            // if cols.len() != 3 {
            //     println!("Invalid param row: {}", row1.inner_html());
            //     return None;
            // }
            let (param_col, type_col, mode_col) = row1.select(&cellselector).next_tuple()?;
            let param_name: String = param_col
                .select(&codeselector)
                .flat_map(|code| code.text())
                .collect();
            // FIXME: make regex static
            let (_, longname, shortname) = regex::Regex::new(r"(\w+)\((\w+)\)")
                .unwrap()
                .captures(&param_name)?
                .iter()
                .filter_map(|m| Some(m?.as_str().to_string()))
                .next_tuple()?;
            let type_name = type_col
                .select(&codeselector)
                .flat_map(|code| code.text())
                .map(convert_type_name)
                .collect();
            let modes = mode_col
                .select(&imgselector)
                .filter_map(|img| img.value().attr("title"))
                .map(|mode| match mode {
                    "query" => FuncMode::Query,
                    "edit" => FuncMode::Edit,
                    _ => FuncMode::Create,
                })
                .collect();
            let description: String = row2
                .select(&cellselector)
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
fn process_file(filename: &str) -> Result<(), Box<dyn Error>> {
    let html_body: String = fs::read_to_string(filename)?.parse()?;
    let document = scraper::Html::parse_document(&html_body);
    // let flags_selector = Selector::parse("").unwrap();
    // let flags = document.select(&flags_selector).nth(0).unwrap();

    let tbody_selector = Selector::parse("a ~ table").unwrap();
    let tbody = document.select(&tbody_selector).nth(0).unwrap();
    let params = process_params_table(tbody);
    Ok(())
}
fn main() {
    match process_file("./source_docs/2023/CommandsPython/xformConstraint.html") {
        Err(e) => panic!("{:?}", e),
        Ok(()) => (),
    }
}
