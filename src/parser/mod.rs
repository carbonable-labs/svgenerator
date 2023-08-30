use itertools::Itertools;
use std::{collections::HashMap, fmt::Display};

use pest::iterators::Pair;
use pest_derive::Parser;
use thiserror::Error;

#[derive(Parser, Debug)]
#[grammar = "parser/svg.pest"]
pub struct SvgParser {}

/// Traverse svg tree and call callback on each iteration
/// pair - [`Pair<Rule>`] - parsed pair
/// cb - [`FnOnce(&Pair<Rule>)`] - Callback on which you apply elements
///
pub fn traverse_tree<Cb>(pair: Pair<Rule>, previous: Option<&Pair<Rule>>, cb: &mut Cb)
where
    Cb: FnMut(&Pair<Rule>, Option<&Pair<Rule>>),
{
    cb(&pair, previous);
    for inner_pair in pair.clone().into_inner() {
        traverse_tree(inner_pair, Some(&pair), cb);
    }
}

#[derive(Error, Debug)]
pub enum SvgElementError {
    #[error("not a root element")]
    NotRoot,
    #[error("no attribute key in attribute_item")]
    NoAttrKey,
    #[error("no attribute value in attribute_item")]
    NoAttrValue,
    #[error("no tag found in element")]
    NoTag,
}

#[derive(Debug, Clone)]
pub struct SvgElement {
    pub outer: String,
    pub tag: String,
    pub attributes: HashMap<String, String>,
    pub replacement: HashMap<String, String>,
    pub nodes: Vec<SvgElement>,
}

impl TryFrom<Pair<'_, Rule>> for SvgElement {
    type Error = SvgElementError;

    fn try_from(value: Pair<'_, Rule>) -> Result<Self, Self::Error> {
        let rule = value.as_rule();
        if ![Rule::root, Rule::nested_element, Rule::single_element].contains(&rule) {
            return Err(SvgElementError::NotRoot);
        }

        let mut outer = value.as_str().to_owned();
        let mut tag = None;
        let mut attributes = HashMap::new();
        let mut replacement = HashMap::new();
        let mut nodes = Vec::new();

        for tags in value.into_inner() {
            match tags.as_rule() {
                Rule::tag => {
                    tag = Some(tags.as_str());
                }
                Rule::attributes => {
                    for attr in tags.into_inner() {
                        let mut attr_item = attr.into_inner();
                        let key = attr_item.next().ok_or(SvgElementError::NoAttrKey)?;
                        let value = attr_item.next().ok_or(SvgElementError::NoAttrValue)?;
                        if Rule::replacement_token == value.as_rule() {
                            replacement.insert(key.as_str().to_owned(), value.as_str().to_owned());
                            continue;
                        }
                        attributes.insert(key.as_str().to_owned(), value.as_str().to_owned());
                    }
                }
                Rule::children => {
                    for node in tags.into_inner() {
                        if Rule::replacement_token == node.as_rule() {
                            replacement.insert("content".to_owned(), node.as_str().to_owned());
                            continue;
                        }
                        if Rule::content == node.as_rule() {
                            replacement.insert("content".to_owned(), node.as_str().to_owned());
                            continue;
                        }
                        if Rule::comment == node.as_rule() {
                            continue;
                        }

                        if node_has_children_to_nest(&node)? {
                            let element = SvgElement::try_from(node)?;
                            nodes.push(element);
                        }
                    }
                }
                _ => (),
            }
        }

        for n in nodes.iter() {
            outer = outer.replace(n.get_plain_outer().as_str(), "{inner_nodes}");
        }

        Ok(SvgElement {
            outer,
            tag: tag.ok_or(SvgElementError::NoTag)?.to_owned(),
            attributes,
            replacement,
            nodes,
        })
    }
}

impl SvgElement {
    pub fn get_plain_outer(&self) -> String {
        let nodes_plain_outer: Vec<String> =
            self.nodes.iter().map(|n| n.get_plain_outer()).collect();

        self.outer
            .split("{inner_nodes}")
            .collect::<Vec<&str>>()
            .into_iter()
            .map(|s| s.to_owned())
            .interleave(nodes_plain_outer)
            .collect::<Vec<String>>()
            .join("")
    }
}

/// Go further into node children to check if any descendent
/// is matching the group rule. [g, defs, filter,]
/// * node - [`&Pair<Rule>`] - parsed pair
///
fn node_has_children_to_nest(node: &Pair<Rule>) -> Result<bool, SvgElementError> {
    let rule = node.as_rule();
    let mut node_iter = node.clone().into_inner();

    while let Some(n) = node_iter.next() {
        if Rule::nested_element == rule && ["svg", "g", "defs", "filter"].contains(&n.as_str()) {
            return Ok(true);
        }
    }

    Ok(false)
}

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, fs::read_to_string};

    use crate::parser::{traverse_tree, Rule, SvgParser};
    use pest::{iterators::Pair, Parser};

    use super::SvgElement;

    #[test]
    fn it_parses_root_attribute() {
        let input = r#"<svg></svg>"#;
        let mut root_pair = SvgParser::parse(Rule::root, input).unwrap();
        assert_eq!(1, root_pair.len());
        let root = root_pair.next().unwrap();
        let mut root_inner = root.into_inner();

        assert_eq!(3, root_inner.len());
        assert_eq!(Rule::tag, root_inner.next().unwrap().as_rule());
        assert_eq!(Rule::attributes, root_inner.next().unwrap().as_rule());
        assert_eq!(Rule::tag_closing, root_inner.next().unwrap().as_rule());
    }

    #[test]
    fn it_doesnt_duplicate_outer() {
        let input = r#"<svg><path d="0 0 0 0" /><path d="1 1 1 1" /><g><text><tspan>Test text</tspan></text><g><text><tspan>Nested text</tspan></text></g></g><path d="2 2 2 2 " /><path d="3 3 3 3" /></svg>"#;
        let mut root_pair = SvgParser::parse(Rule::root, input).unwrap();
        let root = root_pair.next().unwrap();

        let svg = SvgElement::try_from(root).unwrap();
        println!("{:#?}", svg);
        assert_eq!(1, svg.nodes.len());

        println!("{}", svg.to_string());
    }

    #[test]
    fn it_parses_attributes() {
        let input = r#"<svg width="316" height="360" viewBox="0 0 316 360" fill="none" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"></svg>"#;
        let mut root_pair = SvgParser::parse(Rule::root, input).unwrap();
        assert_eq!(1, root_pair.len());
        let root = root_pair.next().unwrap();
        let mut root_inner = root.into_inner();

        assert_eq!(3, root_inner.len());
        let svg_tag_begin = root_inner.next().unwrap();
        let attributes = root_inner.next().unwrap();
        let svg_tag_closing = root_inner.next().unwrap();

        assert_eq!(Rule::tag, svg_tag_begin.as_rule());
        assert_eq!(Rule::attributes, attributes.as_rule());
        assert_eq!(Rule::tag_closing, svg_tag_closing.as_rule());

        assert_eq!(6, attributes.into_inner().len());
    }

    #[test]
    fn it_parses_tree() {
        let input = r#"<svg width="316" height="360" viewBox="0 0 316 360" fill="none" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
        <g filter="url(#filter0_d_2804_179677)">
            <g clip-path="url(#clip0_2804_179677)">
                <path d="M503 11C503 6.58172 499.418 3 495 3H-86C-90.4183 3 -94 6.58172 -94 11V356H503V11Z" fill="url(#pattern0)"/>
                <path d="M503 11C503 6.58172 499.418 3 495 3H-86C-90.4183 3 -94 6.58172 -94 11V356H503V11Z" fill="url(#paint0_linear_2804_179677)"/></g></g>
        <g filter="url(#filter1_b_2804_179677)">
            <rect x="4" y="4" width="308" height="356" fill="url(#paint1_linear_2804_179677)" fill-opacity="0.01"/>
        </g>
    </svg>"#;

        let mut root_pair = SvgParser::parse(Rule::root, input).unwrap();
        assert_eq!(1, root_pair.len());

        let mut tags_count = 0;
        traverse_tree(root_pair.next().unwrap(), None, &mut |pair, _| {
            let rule = pair.as_rule();
            match rule {
                Rule::tag => tags_count += 1,
                _ => {}
            }
        });

        assert_eq!(7, tags_count);
    }

    #[test]
    fn it_converts_tree_into_elements() {
        let input = r#"<svg width="316" height="360" viewBox="0 0 316 360" fill="none" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"></svg>"#;
        let mut root_pair = SvgParser::parse(Rule::root, input).unwrap();
        let root = root_pair.next().unwrap();

        let svg = SvgElement::try_from(root).unwrap();
        assert_eq!(&svg.tag, "svg");
        assert_eq!(svg.attributes.len(), 6);
        assert_eq!(svg.nodes.len(), 0);
        assert_eq!(&svg.outer, input);
    }

    #[test]
    fn it_converts_nested_tree_into_elements() {
        let input = r#"<svg><g><path id="first-child" /><path id="last-child" /></g></svg>"#;
        let mut root_pair = SvgParser::parse(Rule::root, input).unwrap();
        let root = root_pair.next().unwrap();

        let svg = SvgElement::try_from(root).unwrap();
        assert_eq!(&svg.tag, "svg");
        assert_eq!(svg.attributes.len(), 0);
        assert_eq!(svg.nodes.len(), 1);

        let inner = &svg.nodes[0];
        assert_eq!(&inner.tag, "g");
        assert_eq!(inner.nodes.len(), 0);
    }

    #[test]
    fn it_recurse_only_g() {
        let input = r#"<svg>
            <path id="@@replace_me@@" />
            <text>@@replace_text@@</text>
            <g>
            <text>
            <tspan>Test1</tspan>
            </text>
            <text>
            <tspan>Test2</tspan>
            </text>
            <text>
            <tspan>Test3</tspan>
            </text>
            <g>
            <text>
            <tspan>Some nested text</tspan>
            </text>
            <g>
            <text>
            <tspan>Some nest inside nest</tspan>
            </text>
            </g>
            </g>
            </g>
            <defs>
            <filter id="e" width="304" height="69" x="6" y="231" color-interpolation-filters="sRGB" filterUnits="userSpaceOnUse">
      <feFlood flood-opacity="0" result="BackgroundImageFix" />
      <feColorMatrix in="SourceAlpha" result="hardAlpha" values="0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 127 0" />
      <feOffset dy="8" />
      <feGaussianBlur stdDeviation="3" />
      <feComposite in2="hardAlpha" operator="out" />
      <feColorMatrix values="0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.05 0" />
    </filter>
            </defs>
            </svg>"#.replace("\n", "");
        let mut root_pair = SvgParser::parse(Rule::root, &input).unwrap();
        let root = root_pair.next().unwrap();

        let svg = SvgElement::try_from(root).unwrap();
        assert_eq!(svg.nodes.len(), 2);
    }

    impl SvgElement {
        fn fake(outer: String) -> Self {
            Self {
                outer,
                tag: "fake".to_owned(),
                attributes: HashMap::new(),
                nodes: Vec::new(),
                replacement: HashMap::new(),
            }
        }
        fn add_node(&mut self, node: Self) {
            self.nodes.push(node);
        }
    }

    #[test]
    fn test_get_plain_outer() {
        let nested_nested_nested = SvgElement::fake("test".to_owned());
        let mut nested_nested = SvgElement::fake("this // {inner_nodes} // test".to_owned());
        nested_nested.add_node(nested_nested_nested);
        let mut nested = SvgElement::fake("this @@ {inner_nodes} @@ test".to_owned());
        nested.add_node(nested_nested);
        let mut parent = SvgElement::fake("Im the {inner_nodes} parent".to_owned());
        parent.add_node(nested);

        assert_eq!(
            "Im the this @@ this // test // test @@ test parent",
            parent.get_plain_outer()
        );

        let nested_nested_nested = SvgElement::fake("test".to_owned());
        let mut nested_nested = SvgElement::fake("this // {inner_nodes} // test".to_owned());
        nested_nested.add_node(nested_nested_nested);
        let mut nested = SvgElement::fake("this @@ {inner_nodes} @@ test".to_owned());
        nested.add_node(nested_nested);
        let mut parent = SvgElement::fake("Im the {inner_nodes} parent {inner_nodes}".to_owned());
        parent.add_node(nested.clone());
        parent.add_node(nested);
        assert_eq!("Im the this @@ this // test // test @@ test parent this @@ this // test // test @@ test", parent.get_plain_outer());
    }

    #[test]
    fn test_get_plain_outer_svg() {
        let input = read_to_string("./test1.svg")
            .unwrap()
            .replace("\n", "")
            .replace("    ", "");

        let mut root_pair = SvgParser::parse(Rule::root, &input).unwrap();
        let root = root_pair.next().unwrap();

        let svg = SvgElement::try_from(root).unwrap();
        assert_eq!(svg.get_plain_outer(), input);
    }
}
