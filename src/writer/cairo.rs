use crate::parser::SvgElement;
use rand::Rng;
use regex::Regex;
use std::{
    collections::HashMap,
    fmt::{Display, Write},
};

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
            // parts.push(part);
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
            // parts.push(part);
        }
        parts.push(Part::build_tail_element(value));

        Self { parts, nested }
    }
}

impl Display for CairoProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut program = String::new();
        if 2 == self.parts.len() {
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

#[derive(Debug)]
struct Part {
    name: String,
    value: CairoString,
}

/// Generates a random integer as a String
fn random_int_string() -> String {
    let mut rn = rand::thread_rng();
    let num = rn.gen_range(10000..100000);
    num.to_string()
}

impl Part {
    fn build_head_element(element: &SvgElement) -> Self {
        let mut name = "print_head_".to_owned();
        append_string(&mut name, &element.tag);
        append_string(&mut name, &random_int_string());

        let mut head = "<".to_owned();
        append_string(&mut head, &element.tag);

        let mut attributes = String::new();
        for (key, value) in element.attributes.iter() {
            append_string(&mut attributes, " ");
            append_string(&mut attributes, &key);
            append_string(&mut attributes, "=\"");
            append_string(&mut attributes, &value);
            append_string(&mut attributes, "\"");
        }

        let mut replacements = String::new();
        for (key, value) in element.replacement.iter() {
            append_string(&mut replacements, " ");
            append_string(&mut replacements, &key);
            append_string(&mut replacements, "=\"");
            append_string(&mut replacements, &value);
            append_string(&mut replacements, "\"");
        }

        append_string(&mut head, &replacements);
        append_string(&mut head, &attributes);
        append_string(&mut head, ">");

        Self {
            name,
            value: CairoString::from(head.to_owned()),
        }
    }

    fn build_tail_element(element: &SvgElement) -> Self {
        let mut name = "print_tail_".to_owned();
        append_string(&mut name, &element.tag);
        append_string(&mut name, &random_int_string());

        let mut outer = "</".to_owned();
        append_string(&mut outer, &element.tag);
        append_string(&mut outer, ">");

        Self {
            name,
            value: CairoString::from(outer.to_owned()),
        }
    }
}

impl From<SvgElement> for Part {
    fn from(value: SvgElement) -> Self {
        let mut name = "print_".to_owned();
        append_string(&mut name, &value.tag);
        append_string(&mut name, &random_int_string());

        Self {
            name,
            value: CairoString::from(value.outer),
        }
    }
}

impl From<&SvgElement> for Part {
    fn from(value: &SvgElement) -> Self {
        let mut name = "print_".to_owned();
        append_string(&mut name, &value.tag);
        append_string(&mut name, &random_int_string());

        Self {
            name,
            value: CairoString::from(value.outer.to_owned()),
        }
    }
}

// Turn [`Arguments`] into a cairo function argument string
// * `args` - [`&Arguments`]
//
fn format_arguments(args: &Arguments) -> String {
    args.0
        .iter()
        .map(|(arg_name, (arg_type, _))| {
            let mut arg = arg_name.to_owned();
            append_string(&mut arg, ": ");
            append_string(&mut arg, &arg_type);
            arg
        })
        .collect::<Vec<String>>()
        .join(", ")
}

impl Display for Part {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            r#"fn {}({}) -> Array<felt252> {{
{}
}}"#,
            &self.name,
            format_arguments(&self.value.arguments),
            self.value.to_string(),
        )
    }
}

/// Easy way to append to string without allocating to a new one.
/// - value - [`&mut String`]
/// - append = [`&str`]
///
fn append_string(value: &mut String, append: &str) {
    for (_, ch) in append.chars().enumerate() {
        write!(value, "{}", ch).expect("should succeed write to string");
    }
}

#[derive(Debug)]
pub struct CairoString {
    inner: String,
    arguments: Arguments,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Arguments(HashMap<String, (String, Position)>);
impl From<&str> for Arguments {
    fn from(value: &str) -> Self {
        let mut inner = HashMap::new();
        let expr = Regex::new(r"@@(?P<name>[^@@]*)@@").unwrap();
        for v in expr.captures_iter(value) {
            if let Some(arg_name) = v.name("name") {
                let full_match = v.get(0).expect("should at least have one match");
                let args = &arg_name.as_str().split(":").collect::<Vec<&str>>()[..];
                let arg_name = args[0];
                let arg_type = if args.len() > 1 { args[1] } else { "felt252" };

                let arg_type = (arg_type.to_owned(), Position::from(full_match));

                inner.insert(arg_name.to_owned(), arg_type);
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

impl Display for CairoString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut head = String::from("let string = ArrayTrait::new();\n");

        // replace all tokens with a special char to split string at.
        // add new line with string.append(var);
        let mut str_with_args = Vec::new();
        let mut arg_names = Vec::new();
        let mut last_pos = 0;

        for (arg_name, (_, pos)) in self.arguments.0.iter() {
            let str_part = &self.inner[last_pos..pos.start];
            str_with_args.push(str_part.to_string());
            str_with_args.push(arg_name.to_string());
            last_pos = pos.end;
            arg_names.push(arg_name.to_string());
        }
        // add last str_part
        let str_part = &self.inner[last_pos..];
        str_with_args.push(str_part.to_string());

        for part in str_with_args.iter() {
            let str_chars = part.chars().collect::<Vec<char>>();
            for str in str_chars.as_slice().chunks(32) {
                head.extend("string.append(".chars());
                if !arg_names.contains(part) {
                    head.extend(['\'']);
                }
                head.extend(str);
                if !arg_names.contains(part) {
                    head.extend(['\'']);
                }
                head.extend([')', ';', '\n'])
            }
        }
        head.extend(['s', 't', 'r', 'i', 'n', 'g']);
        f.write_str(&head)
    }
}

#[cfg(test)]
mod tests {
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

        let cairo_string = CairoString::from(input);
        let expected = r#"let string = ArrayTrait::new();
string.append('<svg width="316" height="360" vi');
string.append('ewBox="0 0 316 360" fill="none" ');
string.append('xmlns="http://www.w3.org/2000/sv');
string.append('g" xmlns:xlink="http://www.w3.or');
string.append('g/1999/xlink"></svg>');
string"#;

        assert_eq!(expected, cairo_string.to_string());
    }

    #[test]
    fn test_from_svg_element() {
        let input = r#"<svg></svg>"#;
        let mut root_pair = SvgParser::parse(Rule::root, input).unwrap();
        let root = root_pair.next().unwrap();

        let svg = SvgElement::try_from(root).unwrap();
        let cairo_program = CairoProgram::from(svg);

        // check only if there is the expected number of function prints
        regex_match_res("print_", 1, &cairo_program.to_string());
    }

    #[test]
    fn test_it_splits_function() {
        let input = r#"<svg><path d="M0 M0 M0" /></svg>"#;
        let mut root_pair = SvgParser::parse(Rule::root, input).unwrap();
        let root = root_pair.next().unwrap();

        let svg = SvgElement::try_from(root).unwrap();
        let cairo_program = CairoProgram::from(svg);

        assert_eq!(2, cairo_program.parts.len());
    }

    #[test]
    fn test_it_adds_attributes() {
        let input = r#"<svg width="316"><path d="M0 M0 M0" /></svg>"#;
        let mut root_pair = SvgParser::parse(Rule::root, input).unwrap();
        let root = root_pair.next().unwrap();

        let svg = SvgElement::try_from(root).unwrap();
        let cairo_program = CairoProgram::from(svg);

        assert_eq!(2, cairo_program.parts.len());

        // only check expected function count
        regex_match_res("print_", 3, &cairo_program.to_string());
    }

    #[test]
    fn test_nested_tree() {
        let input = r#"<svg width="316"><g><path d="M0 M0 M0" /></g></svg>"#;
        let mut root_pair = SvgParser::parse(Rule::root, input).unwrap();
        let root = root_pair.next().unwrap();

        let svg = SvgElement::try_from(root).unwrap();
        let cairo_program = CairoProgram::from(svg);

        assert_eq!(2, cairo_program.parts.len());

        // as cairo_program builds fn names with random int at the end. We only check if we get the
        // expected function number
        regex_match_res("print_", 5, &cairo_program.to_string());
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
        let cairo_program = CairoProgram::from(&svg);

        // as cairo_program builds fn names with random int at the end. We only check if we get the
        // expected function number
        regex_match_res("print_", 6, &cairo_program.to_string());
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

        assert_eq!(arguments.0.get("argument_1").unwrap().0.as_str(), "felt252");
        assert_eq!(arguments.0.get("argument2").unwrap().0.as_str(), "felt252");
    }

    #[test]
    fn test_argument_with_concrete_type() {
        let input = "@@argument1:ConcreteType@@";
        let arguments = Arguments::from(input);

        assert_eq!(
            arguments.0.get("argument1").unwrap().0.as_str(),
            "ConcreteType"
        );
    }

    #[test]
    fn test_cairo_string_with_arguments() {
        let input = r#"<svg width="@@starknet_id@@"></svg>"#;
        let cairo_string = CairoString::from(input);
        let expected = r#"let string = ArrayTrait::new();
string.append('<svg width="');
string.append(starknet_id);
string.append('"></svg>');
string"#;

        assert_eq!(expected, cairo_string.to_string());
    }
}
