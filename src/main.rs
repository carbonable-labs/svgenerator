use std::{
    fs::read_to_string,
    path::{Path, PathBuf},
};

use pest::Parser;
use regex::{Captures, Regex, Replacer};

use crate::parser::{SvgElement, SvgParser};
use crate::writer::{ConsoleWriter, FileWriter, Writer};

mod parser;
mod writer;

fn main() -> anyhow::Result<()> {
    let cmd = create_command();

    let matches = cmd.get_matches();

    match matches.subcommand() {
        Some(("dry-run", matches)) => {
            let path = matches.get_one::<std::path::PathBuf>("PATH");
            handle_file(
                path.expect("should at least have one path"),
                true,
                true,
                ConsoleWriter {},
            )?;
            println!("{:#?}", path);
        }
        Some(("generate", matches)) => {
            let path = matches.get_one::<std::path::PathBuf>("PATH");
            let quote_escape = matches.get_flag("escaped");
            let html_escape = matches.get_flag("html");
            handle_file(
                path.expect("should at least have one path"),
                quote_escape,
                html_escape,
                FileWriter {},
            )?;
            println!("{:#?}", path);
        }
        Some(("groups", matches)) => {
            let path = matches.get_one::<std::path::PathBuf>("PATH");
            structure(path.expect("should at least have one path"))?;
        }
        _ => unreachable!("clap should ensure we don't get here"),
    };

    Ok(())
}

fn print_structure(svg: SvgElement) {
    println!("SVG");
    println!("├── {}", svg.tag);
    for (key, value) in svg.attributes {
        println!("│   ├── {}: {}", key, value);
    }
    for node in svg.nodes {
        print_node(node, 1);
    }
}

fn print_node(node: SvgElement, level: usize) {
    let mut prefix = String::new();
    for _ in 0..level {
        prefix.push_str("│   ");
    }
    if node.tag == "g" && node.attributes.contains_key("id") {
        println!(
            "{}├── group: {}",
            prefix,
            node.attributes.get("id").unwrap()
        );
    } else {
        println!("{}├── {}", prefix, node.tag);
    }
    for (key, value) in node.attributes {
        if key == "id" {
            continue;
        }
        println!("{}│   ├── {}: {}", prefix, key, value);
    }
    for node in node.nodes {
        print_node(node, level + 1);
    }
}

fn structure<P: AsRef<Path>>(path: P) -> anyhow::Result<()> {
    let unparsed_file = read_to_string(&path)
        .expect("cannot read file")
        .replace("\n", "")
        .replace("    ", "");
    let mut parsed = SvgParser::parse(parser::Rule::root, &unparsed_file).unwrap();
    let document = parsed.next().unwrap();
    let svg = SvgElement::try_from(document).unwrap();

    print_structure(svg);

    Ok(())
}

struct Unescape;

impl Replacer for Unescape {
    fn replace_append(&mut self, caps: &Captures<'_>, dest: &mut String) {
        let text = caps["s"].to_string();
        let re = Regex::new(r#"\\\\"#).unwrap();
        let res = re.replace_all(&text, "");
        dest.push('\'');
        dest.push_str(&res);
        dest.push('\'');
    }
}

/// Handle inputed files
fn handle_file<P: AsRef<Path>>(
    path: P,
    quote_escape: bool,
    html_escape: bool,
    writer: impl Writer,
) -> anyhow::Result<()> {
    let mut unparsed_file = read_to_string(&path)
        .expect("cannot read file")
        .replace("\n", "");

    let re = Regex::new(r"\s+").unwrap();
    unparsed_file = re.replace_all(&unparsed_file, " ").to_string();

    let re = Regex::new(r">\s+<").unwrap();
    unparsed_file = re.replace_all(&unparsed_file, "><").to_string();
    let re = Regex::new(r">\s+@").unwrap();
    unparsed_file = re.replace_all(&unparsed_file, ">@").to_string();
    let re = Regex::new(r"@\s+<").unwrap();
    unparsed_file = re.replace_all(&unparsed_file, "@<").to_string();

    if html_escape {
        let re = Regex::new(r"#").unwrap();
        unparsed_file = re.replace_all(&unparsed_file, "%23").to_string();
    }
    let mut parsed = SvgParser::parse(parser::Rule::root, &unparsed_file).unwrap();
    let document = parsed.next().unwrap();
    let svg = SvgElement::try_from(document).unwrap();

    let file = path.as_ref().as_os_str().to_str().unwrap();
    let header = format!(
        "\
////////////////////////////////////////\n\
//            SVGenerated             //\n\
////////////////////////////////////////\n\
// source file: {}\n",
        file
    );
    let name = handle_file_name(file);
    let mut contents: String = String::from(header);
    let mut cairo_svg = svg.to_string();

    if !quote_escape {
        let re = Regex::new(r"'(?<s>[^']*)'").unwrap();
        cairo_svg = re.replace_all(&cairo_svg, Unescape {}).to_string();
    }
    contents.push_str(cairo_svg.as_str());
    let _ = writer.write(&name, contents.as_str())?;
    Ok(())
}

fn handle_file_name(name: &str) -> String {
    let name = name.to_string();
    name.replace(".svg", ".cairo")
}

/// Create command instance to parse out inputed files
///
fn create_command() -> clap::Command {
    clap::Command::new("svgenerator")
        .bin_name("svgenerator")
        .subcommand_required(true)
        .subcommand(
            clap::Command::new("dry-run")
                .about("Check the expected output")
                .arg(
                    clap::arg!(<PATH> ... "The file to parse out.")
                        .value_parser(clap::value_parser!(PathBuf)),
                )
                .arg_required_else_help(true),
        )
        .subcommand(
            clap::Command::new("generate")
                .about("Generate the files output")
                .arg(
                    clap::arg!(<PATH> ... "The file to parse out.")
                        .value_parser(clap::value_parser!(PathBuf)),
                )
                .arg_required_else_help(true)
                .arg(clap::arg!(-e --escaped "Optionally escape quotes"))
                .arg(clap::arg!(-z --html "Optionally toggle HTML compatibility")),
        )
        .subcommand(
            clap::Command::new("groups")
                .about("Generate the SVG group structure")
                .arg(
                    clap::arg!(<PATH> ... "The file to parse out.")
                        .value_parser(clap::value_parser!(PathBuf)),
                )
                .arg_required_else_help(true),
        )
}
