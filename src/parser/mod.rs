use std::collections::HashMap;

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

#[derive(Debug)]
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

        let outer = value.as_str().to_owned();
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

                        let element = SvgElement::try_from(node)?;
                        nodes.push(element);
                    }
                }
                _ => (),
            }
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

#[cfg(test)]
mod tests {
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
        assert_eq!(&svg.outer, input);
        let inner = &svg.nodes[0];
        assert_eq!(&inner.tag, "g");
        assert_eq!(inner.nodes.len(), 2);
    }

    #[test]
    fn it_can_parse_replacement_token() {
        let input = r#"<svg><g><path id="@@replace_me@@" /></g></svg>"#;
        let mut root_pair = SvgParser::parse(Rule::root, input).unwrap();
        let root = root_pair.next().unwrap();

        let svg = SvgElement::try_from(root).unwrap();
        let first_node = &svg.nodes[0].nodes[0];
        assert_eq!(1, first_node.replacement.len());
        assert_eq!(0, first_node.attributes.len())
    }

    #[test]
    fn it_can_parse_replacement_token_in_content() {
        let input = r#"<svg><path id="@@replace_me@@" /><text>@@replace_text@@</text></svg>"#;
        let mut root_pair = SvgParser::parse(Rule::root, input).unwrap();
        let root = root_pair.next().unwrap();

        let svg = SvgElement::try_from(root).unwrap();
        let text_node = &svg.nodes[1];

        assert_eq!(1, text_node.replacement.len());
        assert_eq!(0, text_node.attributes.len());
        assert_eq!(
            &"@@replace_text@@".to_owned(),
            text_node.replacement.get("content").unwrap()
        );
    }
}
