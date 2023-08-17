use std::{
    fs::read_to_string,
    path::{Path, PathBuf},
};

use pest::Parser;

use crate::parser::SvgParser;

mod parser;
mod writer;

fn main() -> anyhow::Result<()> {
    let cmd = create_command();

    let matches = cmd.get_matches();

    match matches.subcommand() {
        Some(("dry-run", matches)) => {
            let path = matches.get_one::<std::path::PathBuf>("PATH");
            handle_file(path.expect("should at least have one path"))?;
            println!("{:#?}", path);
        }
        Some(("generate", matches)) => {
            let path = matches.get_one::<std::path::PathBuf>("PATH");
            println!("{:#?}", path);
        }
        _ => unreachable!("clap should ensure we don't get here"),
    };

    Ok(())
}

/// Handle inputed files
fn handle_file<P: AsRef<Path>>(path: P) -> anyhow::Result<()> {
    let unparsed_file = read_to_string(path).expect("cannot read file");
    let mut parsed = SvgParser::parse(parser::Rule::root, &unparsed_file).unwrap();
    let document = parsed.next().unwrap();
    assert!(parser::Rule::root == document.as_rule());

    Ok(())
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
                .arg_required_else_help(true),
        )
}
