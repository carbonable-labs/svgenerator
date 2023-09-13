use crate::parser::SvgElement;
use itertools::Itertools;
use rand::Rng;
use regex::Regex;
use std::{
    collections::BTreeMap,
    fmt::{Display, Write},
};

impl Display for SvgElement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut prelude = String::new();
        let parts: Vec<CairoString> = self
            .outer
            .split("{inner_nodes}")
            .map(|p| CairoString::from(p))
            .collect();
        let head = Part::build_head_element(self);
        let mut body: BTreeMap<usize, Part> = BTreeMap::new();

        let mut node_iter = self.nodes.iter().enumerate();
        while let Some((i, node)) = node_iter.next() {
            if !node.nodes.is_empty() {
                let node_str = node.to_string();
                let prelude_fn_name = get_prelude_fn_name(&node_str);
                body.insert(
                    i,
                    Part::from(node)
                        .with_fn_name(prelude_fn_name)
                        .a_function_call(),
                );
                append_string(&mut prelude, &node_str);
                continue;
            }
            body.insert(i, Part::from(node));
        }

        let function_name = get_function_name_from_part(&body);

        write!(
            f,
            r#"{}

{}

#[inline(always)]
fn {}(ref svg: Array<felt252>{}) {{
{}
}}"#,
            prelude,
            body.iter()
                .filter(|p| !p.1.as_function_call)
                .map(|p| p.1.to_string())
                .collect::<Vec<String>>()
                .join("\n"),
            head.name.to_string(),
            print_function_required_arguments(
                parts.as_slice(),
                body.iter()
                    .filter(|p| !p.1.as_function_call)
                    .map(|p| p.1)
                    .collect::<Vec<&Part>>()
                    .as_slice()
            ),
            print_function_call(parts.as_slice(), function_name.as_slice(),),
        )
    }
}

fn get_prelude_fn_name(prelude: &str) -> &str {
    let re = Regex::new("fn (?P<fn_name>print_[^(]*)").expect("failed to parse out regex");
    let matches: Vec<regex::Captures> = re.captures_iter(prelude).collect();
    let name = matches
        .last()
        .expect("should have fn name")
        .name("fn_name")
        .expect("should have matched")
        .as_str();
    name
}

fn get_function_name_from_part(part: &BTreeMap<usize, Part>) -> Vec<String> {
    part.iter()
        .map(|p| p.1.print_function_call().to_owned())
        .collect()
}

fn print_function_call(parts: &[CairoString], fn_name: &[String]) -> String {
    let str_parts: Vec<String> = parts.iter().map(|p| p.to_string()).collect();
    let fn_calls: Vec<String> = fn_name.iter().map(|f| format!("\t{}", f)).collect();
    let calls = str_parts.into_iter().interleave(fn_calls);
    calls.collect::<Vec<String>>().join("\n")
}

fn print_function_required_arguments(parts: &[CairoString], body: &[&Part]) -> String {
    let parts_args: Vec<String> = parts
        .iter()
        .map(|p| format_arguments(&p.arguments))
        .filter(|v| !v.is_empty())
        .collect();
    let body_args: Vec<String> = body
        .iter()
        .map(|p| format_arguments(&p.value.arguments))
        .filter(|v| !v.is_empty())
        .collect();

    let merged_parts: Vec<String> = parts_args.into_iter().interleave(body_args).collect();

    if 0 < merged_parts.len() {
        return ", data: @Data".to_owned();
    }

    "".to_owned()
}

#[derive(Debug)]
pub struct CairoProgram {
    parts: Vec<Part>,
    nested: Vec<Self>,
}

impl From<SvgElement> for CairoProgram {
    fn from(value: SvgElement) -> Self {
        let mut parts = Vec::new();
        let mut nested = Vec::new();

        if value.nodes.is_empty() {
            let part = Part::from(&value);
            parts.push(part);
            return Self {
                parts,
                nested: vec![],
            };
        }

        parts.push(Part::build_head_element(&value));

        for node in value.nodes.iter() {
            nested.push(CairoProgram::from(node));
        }

        parts.push(Part::build_tail_element(&value));

        Self { parts, nested }
    }
}
impl From<&SvgElement> for CairoProgram {
    fn from(value: &SvgElement) -> Self {
        let mut parts = Vec::new();
        let mut nested = Vec::new();
        if value.nodes.is_empty() {
            let part = Part::from(value);
            parts.push(part);
            return Self {
                parts,
                nested: vec![],
            };
        }

        parts.push(Part::build_head_element(value));

        for node in value.nodes.iter() {
            nested.push(CairoProgram::from(node));
        }

        parts.push(Part::build_tail_element(value));

        Self { parts, nested }
    }
}

impl Display for CairoProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut program = String::new();
        if 2 <= self.parts.len() {
            let head: String = unsafe { self.parts.get_unchecked(0).to_string() };
            append_string(&mut program, &head);
            append_string(&mut program, "\n");
            for nested in &self.nested {
                let inner: String = nested.to_string();
                append_string(&mut program, &inner);
            }
            let tail: String = unsafe { self.parts.get_unchecked(1).to_string() };
            append_string(&mut program, &tail);
            append_string(&mut program, "\n");

            return f.write_str(&program);
        }

        for part in &self.parts {
            let part: String = part.to_string();
            append_string(&mut program, &part);
            append_string(&mut program, "\n");
        }

        f.write_str(&program)
    }
}

#[derive(Debug, Default, Clone)]
struct Part {
    name: String,
    value: CairoString,
    as_function_call: bool,
}

/// Generates a random integer as a String
fn random_int_string() -> String {
    let mut rn = rand::thread_rng();
    let num = rn.gen_range(10000..100000);
    num.to_string()
}

fn get_element_name(element: &SvgElement) -> String {
    let mut name = random_int_string();
    if element.attributes.contains_key("id") {
        name = element.attributes["id"].clone();
    }
    name
}

impl Part {
    fn build_head_element(element: &SvgElement) -> Self {
        let mut name = "print_head_".to_owned();
        // append_string(&mut name, &element.tag);
        let suffix = get_element_name(element);
        append_string(&mut name, &suffix);

        let head = element
            .outer
            .split("{inner_nodes}")
            .next()
            .expect("should be splittable into two parts");

        Self {
            name,
            value: CairoString::from(head.to_owned()),
            as_function_call: false,
        }
    }

    fn build_tail_element(element: &SvgElement) -> Self {
        let mut name = "print_tail_".to_owned();
        // append_string(&mut name, &element.tag);
        let suffix = get_element_name(element);
        append_string(&mut name, &suffix);

        let tail = element
            .outer
            .split("{inner_nodes}")
            .last()
            .expect("should be splittable into two parts");

        Self {
            name,
            value: CairoString::from(tail.to_owned()),
            as_function_call: false,
        }
    }

    fn print_function_call(&self) -> String {
        format!("{}(ref svg, data);", &self.name)
    }

    fn a_function_call(mut self) -> Part {
        self.as_function_call = true;
        self
    }

    fn with_fn_name(mut self, name: &str) -> Part {
        self.name = name.to_owned();
        self
    }
}

impl From<SvgElement> for Part {
    fn from(value: SvgElement) -> Self {
        let mut name = "print_".to_owned();
        //append_string(&mut name, &value.tag);
        let suffix = get_element_name(&value);
        append_string(&mut name, &suffix);

        Self {
            name,
            value: CairoString::from(value.outer),
            as_function_call: false,
        }
    }
}

impl From<&SvgElement> for Part {
    fn from(value: &SvgElement) -> Self {
        let mut name = "print_".to_owned();
        // append_string(&mut name, &value.tag);
        let suffix = get_element_name(&value);
        append_string(&mut name, &suffix);

        Self {
            name,
            value: CairoString::from(value.outer.to_owned()),
            as_function_call: false,
        }
    }
}

// Turn [`Arguments`] into a cairo function argument string
// * `args` - [`&Arguments`]
//
fn format_arguments(args: &Arguments) -> String {
    args.0
        .iter()
        .map(|(_, (arg_name, arg_type, _))| {
            let mut arg = arg_name.to_owned();
            append_string(&mut arg, ": ");
            append_string(&mut arg, &arg_type);
            arg
        })
        .collect::<Vec<String>>()
        .join(", ");
    String::from("data: Data")
}

impl Display for Part {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            r#"#[inline(always)]
fn {}(ref svg: Array<felt252>, data: @Data) {{
{}
}}"#,
            &self.name,
            self.value.to_string(),
        )
    }
}

/// Easy way to append to string without allocating to a new one.
/// - value - [`&mut String`]
/// - append = [`&str`]
///
pub fn append_string(value: &mut String, append: &str) {
    for (_, ch) in append.chars().enumerate() {
        write!(value, "{}", ch).expect("should succeed write to string");
    }
}

#[derive(Debug, Default, Clone)]
pub struct CairoString {
    inner: String,
    arguments: Arguments,
}

#[derive(Debug, Clone)]
pub struct Position {
    start: usize,
    end: usize,
}
impl From<regex::Match<'_>> for Position {
    fn from(value: regex::Match<'_>) -> Self {
        Self {
            start: value.start(),
            end: value.end(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Arguments(BTreeMap<usize, (String, String, Position)>);
impl From<&str> for Arguments {
    fn from(value: &str) -> Self {
        let mut inner = BTreeMap::new();
        let expr = Regex::new(r"@@(?P<name>[^@@]*)@@").unwrap();
        for (i, v) in expr.captures_iter(value).enumerate() {
            if let Some(arg_name) = v.name("name") {
                let full_match = v.get(0).expect("should at least have one match");
                let args = &arg_name.as_str().split(":").collect::<Vec<&str>>()[..];
                let arg_name = args[0];
                let arg_type = if args.len() > 1 { args[1] } else { "felt252" };

                inner.insert(
                    i,
                    (
                        arg_name.to_owned(),
                        arg_type.to_owned(),
                        Position::from(full_match),
                    ),
                );
            }
        }
        Self(inner)
    }
}
impl Default for Arguments {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl From<&str> for CairoString {
    fn from(value: &str) -> Self {
        Self {
            inner: value.to_string(),
            arguments: Arguments::from(value),
        }
    }
}

impl From<String> for CairoString {
    fn from(value: String) -> Self {
        Self {
            inner: value.to_string(),
            arguments: Arguments::from(value.as_str()),
        }
    }
}

impl From<Vec<SvgElement>> for CairoString {
    fn from(value: Vec<SvgElement>) -> Self {
        let elements = value
            .iter()
            .map(|e| e.outer.as_str())
            .collect::<Vec<&str>>()
            .join("")
            .to_string();
        Self {
            inner: elements.to_owned(),
            arguments: Arguments::from(elements.as_str()),
        }
    }
}

impl Display for CairoString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut head = String::from("");

        // replace all tokens with a special char to split string at.
        // add new line with string.append(data.var);
        let mut str_with_args = Vec::new();
        let mut arg_names = Vec::new();
        let mut last_pos = 0;

        for (_, (arg_name, _, pos)) in self.arguments.0.iter() {
            let str_part = &self.inner[last_pos..pos.start];
            str_with_args.push(str_part.to_string());
            str_with_args.push(arg_name.to_string());
            last_pos = pos.end;
            arg_names.push(arg_name.to_string());
        }
        // add last str_part
        let str_part = &self.inner[last_pos..];
        str_with_args.push(str_part.to_string());

        for (i, part) in str_with_args.iter().enumerate() {
            if 0 != i {
                head.extend(['\n']);
            }
            // Escape quotes for JSON in Cairo
            let part = &part.replace('"', "\\\\\"");

            // Make chunks of length at most 31 chars
            // but don't end with a single backslash

            let chunk_sizes =
                part.chars()
                    .collect::<Vec<char>>()
                    .iter()
                    .fold(vec![(0, false)], |mut acc, c| {
                        let (size, prev_escaped) = acc.last_mut().unwrap();
                        let new_escaped = c == &'\\';
                        if 31 == *size {
                            acc.push((1, new_escaped));
                            acc
                        } else if 30 == *size && new_escaped && !*prev_escaped {
                            acc.push((1, new_escaped));
                            acc
                        } else if 30 == *size && &' ' == c {
                            acc.push((1, false));
                            acc
                        } else {
                            *size += 1;
                            *prev_escaped = new_escaped;
                            acc
                        }
                    });

            let mut nodes = chunk_sizes.iter().enumerate();
            let mut prev_index = 0;

            while let Some((i, (size, _))) = nodes.next() {
                if 0 != i {
                    head.extend(['\n']);
                }

                let str = part[prev_index..*size + prev_index].chars();
                prev_index += *size;
                if !arg_names.contains(part) {
                    head.extend("\tsvg.append(".chars());
                    head.extend(['\'']);
                    head.extend(str);
                    head.extend(['\'']);
                } else {
                    head.extend("\tsvg.concat(".chars());
                    head.extend("*data.".chars());
                    head.extend(str);
                }
                head.extend([')', ';']);
            }
        }
        f.write_str(&head)
    }
}

#[cfg(test)]
mod tests {
    use std::fs::read_to_string;

    use crate::parser::{Rule, SvgElement, SvgParser};
    use pest::Parser;
    use regex::Regex;

    use super::{append_string, Arguments, CairoProgram, CairoString};

    fn regex_match_res(regex_pattern: &str, expected_count: usize, real: &str) {
        let re = Regex::new(regex_pattern).expect("failed to parse out regex");
        let matches: Vec<regex::Captures> = re.captures_iter(real).collect();

        assert_eq!(expected_count, matches.len())
    }

    #[test]
    fn test_append_string() {
        let mut str_val = "carbon".to_owned();
        append_string(&mut str_val, "ABLE");

        assert_eq!("carbonABLE", str_val);

        let mut cairo_fn = "fn ".to_owned();
        append_string(&mut cairo_fn, "print_svg");
        append_string(&mut cairo_fn, "() -> Array<felt252> {");
        append_string(&mut cairo_fn, "\n");
        append_string(&mut cairo_fn, "}");

        let expected = r#"fn print_svg() -> Array<felt252> {
}"#;
        assert_eq!(expected, cairo_fn)
    }

    #[test]
    fn test_to_cairo_string() {
        let input = r#"<svg width="316" height="360" viewBox="0 0 316 360" fill="none" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"></svg>"#;

        let cairo_string: CairoString = CairoString::from(input);
        let expected = "\tsvg.append('<svg width=\"316\" height=\"360\" v');\n\tsvg.append('iewBox=\"0 0 316 360\" fill=\"none');\n\tsvg.append('\" xmlns=\"http://www.w3.org/2000');\n\tsvg.append('/svg\" xmlns:xlink=\"http://www.w');\n\tsvg.append('3.org/1999/xlink\"></svg>');";

        assert_eq!(expected, cairo_string.to_string());
    }

    #[test]
    fn test_from_svg_element() {
        let input = r#"<svg></svg>"#;
        let mut root_pair = SvgParser::parse(Rule::root, input).unwrap();
        let root = root_pair.next().unwrap();

        let svg = SvgElement::try_from(root).unwrap();

        // check only if there is the expected number of function prints
        regex_match_res("print_", 1, &svg.to_string());
    }

    #[test]
    fn it_ignore_comments() {
        let input = r#"<svg><!-- should ignore comments --></svg>"#;
        let mut root_pair = SvgParser::parse(Rule::root, input).unwrap();
        let root = root_pair.next().unwrap();
        let svg = SvgElement::try_from(root).unwrap();

        regex_match_res("print_", 1, &svg.to_string());
    }

    #[test]
    fn test_it_adds_attributes() {
        let input = r#"<svg width="316"><path d="M0 M0 M0" /></svg>"#;
        let mut root_pair = SvgParser::parse(Rule::root, input).unwrap();
        let root = root_pair.next().unwrap();

        let svg = SvgElement::try_from(root).unwrap();

        // only check expected function count
        regex_match_res("print_", 1, &svg.to_string());
    }

    #[test]
    fn test_nested_tree() {
        let input = r#"<svg width="316"><g><path d="M0 M0 M0" /></g></svg>"#;
        let mut root_pair = SvgParser::parse(Rule::root, input).unwrap();
        let root = root_pair.next().unwrap();

        let svg = SvgElement::try_from(root).unwrap();

        assert_eq!(1, svg.nodes.len());

        // as cairo_program builds fn names with random int at the end. We only check if we get the
        // expected function number
        regex_match_res("print_", 3, &svg.to_string());
    }

    #[test]
    fn test_it_can_add_arguments() {
        let input = r#"<svg width="@@starknet_id@@"></svg>"#;
        let mut root_pair = SvgParser::parse(Rule::root, input).unwrap();
        let root = root_pair.next().unwrap();

        let svg = SvgElement::try_from(root).unwrap();
        let cairo_program = CairoProgram::from(&svg);

        // as cairo_program builds fn names with random int at the end. We only check if we get the
        // expected function number
        regex_match_res("print_", 1, &cairo_program.to_string());
    }

    #[test]
    fn test_it_can_add_arguments_with_nested_tree() {
        let input = r#"<svg width="@@starknet_id@@"><g><path d="000"/><path d="000"/></g></svg>"#;
        let mut root_pair = SvgParser::parse(Rule::root, input).unwrap();
        let root = root_pair.next().unwrap();

        let svg = SvgElement::try_from(root).unwrap();

        // as cairo_program builds fn names with random int at the end. We only check if we get the
        // expected function number
        regex_match_res("print_", 3, &svg.to_string());
    }

    #[test]
    fn test_cairo_string_can_parse_out_arguments() {
        let input = r#"<svg width="@@starknet_id@@"><g><path d="000"/><path d="000"/></g></svg>"#;
        let cairo_string = CairoString::from(input);

        assert_eq!(1, cairo_string.arguments.0.len());
    }

    #[test]
    fn test_arguments_from_string() {
        let input = "this is a test string @@argument_1@@ with two vars @@argument2@@";
        let arguments = Arguments::from(input);

        assert_eq!(arguments.0.get(&0).unwrap().0.as_str(), "felt252");
        assert_eq!(arguments.0.get(&1).unwrap().0.as_str(), "felt252");
    }

    #[test]
    fn test_argument_with_concrete_type() {
        let input = "@@argument1:ConcreteType@@";
        let arguments = Arguments::from(input);

        assert_eq!(arguments.0.get(&0).unwrap().0.as_str(), "ConcreteType");
    }

    #[test]
    fn test_cairo_string_with_arguments() {
        let input = r#"<svg width="@@starknet_id@@"></svg>"#;
        let cairo_string = CairoString::from(input);
        let expected = "\tsvg.append('<svg width=\"');\n\tsvg.append(starknet_id);\n\tsvg.append('\"></svg>');";

        assert_eq!(expected, cairo_string.to_string());
    }

    #[test]
    fn test_it_nest_only_g_and_svg() {
        let input = r#"<svg><g><path d="0 0 0" /><path d="@@starknet_id@@" /></g><path d="@@carbonable_project_id@@" /><path d="3 3 3" /><text>Test</text><defs><filter /></defs></svg>"#;

        let mut root_pair = SvgParser::parse(Rule::root, input).unwrap();
        let root = root_pair.next().unwrap();

        let svg = SvgElement::try_from(root).unwrap();

        assert_eq!(2, svg.nodes.len());
        println!("{}", svg.to_string());
    }

    #[test]
    fn test_with_real_specified_asset() {
        let input = read_to_string("./test1.svg")
            .unwrap()
            .replace("\n", "")
            .replace("    ", "");

        let mut root_pair = SvgParser::parse(Rule::root, &input).unwrap();
        let root = root_pair.next().unwrap();

        let svg = SvgElement::try_from(root).unwrap();

        assert_eq!(1, svg.nodes.len());
        println!("{}", svg.to_string());
    }
}
