use crate::expression::Iri;
use crate::graph::GraphNode;

use crate::var::VerbList;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ObjectList(pub Vec<GraphNode>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PropertyList(pub Vec<VerbList>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Collection(pub Vec<GraphNode>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RdfLiteral {
    pub literal: String,
    pub descriptor: Option<RdfLiteralDescriptor>,
}

impl RdfLiteral {
    /// creates a complete [`RdfLiteral`]
    pub fn new<T: ToString>(literal: T, descriptor: RdfLiteralDescriptor) -> Self {
        RdfLiteral {
            literal: literal.to_string(),
            descriptor: Some(descriptor),
        }
    }

    /// creates a new [`RdfLiteral`] with a `literal` only
    pub fn literal<T: ToString>(literal: T) -> Self {
        RdfLiteral {
            literal: literal.to_string(),
            descriptor: None,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum RdfLiteralDescriptor {
    LangTag(String),
    IriRef(Iri),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BlankNode {
    Anon,
    Label(String),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TriplesNode {
    Collection(Collection),
    BlankNodePropertyList(PropertyList),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::{blank_node, GraphTerm};

    use crate::triple::{
        blank_node_property_list, collection, object_list, property_list, property_list_not_empty,
    };
    use crate::var::{Var, VarOrIri, VarOrTerm, Verb};

    #[test]
    fn is_blank_node() {
        assert_eq!(blank_node("[]"), Ok(("", BlankNode::Anon)));

        assert_eq!(
            blank_node("_:a.b.c"),
            Ok(("", BlankNode::Label("a.b.c".to_string())))
        );
    }

    #[test]
    fn is_collection() {
        assert_eq!(
            collection("(()())"),
            Ok((
                "",
                Collection(vec![
                    GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::Nil)),
                    GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::Nil))
                ])
            ))
        );
    }

    #[test]
    fn is_object_list() {
        assert_eq!(
            object_list("(),()"),
            Ok((
                "",
                ObjectList(vec![
                    GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::Nil)),
                    GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::Nil))
                ])
            ))
        );
    }

    #[test]
    fn is_property_list() {
        assert_eq!(property_list(""), Ok(("", None)));
    }

    #[test]
    fn is_property_list_not_empty() {
        let mut prop_list = PropertyList(vec![VerbList::new(
            Verb::A,
            ObjectList(vec![
                GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::Nil)),
                GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::Nil)),
            ]),
        )]);

        assert_eq!(
            property_list_not_empty("a (),()"),
            Ok(("", prop_list.clone()))
        );

        assert_eq!(
            property_list_not_empty("a (),();;;"),
            Ok(("", prop_list.clone()))
        );

        assert_eq!(
            property_list_not_empty("a () , ()  \n; ; ;"),
            Ok(("", prop_list.clone()))
        );

        prop_list.0.push(VerbList::new(
            Verb::VarOrIri(VarOrIri::Var(Var::QMark("z".to_string()))),
            ObjectList(vec![
                GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::BooleanLiteral(true))),
                GraphNode::VarOrTerm(VarOrTerm::Var(Var::QMark("name".to_string()))),
            ]),
        ));

        assert_eq!(
            property_list_not_empty("a (),(); ?z true , ?name"),
            Ok(("", prop_list.clone()))
        );

        prop_list.0.push(VerbList::new(
            Verb::VarOrIri(VarOrIri::Iri(Iri::Iri(
                "http://example.org/foaf/aliceFoaf".to_string(),
            ))),
            ObjectList(vec![GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::Iri(
                Iri::Iri("http://example.org/foaf/aliceFoaf".to_string()),
            )))]),
        ));

        assert_eq!(
            property_list_not_empty(
                r#"a (),(); ?z true , ?name ;
            <http://example.org/foaf/aliceFoaf>
            <http://example.org/foaf/aliceFoaf> ;; ;"#
            ),
            Ok(("", prop_list.clone()))
        );
    }

    #[test]
    fn is_blank_node_property_list() {
        assert_eq!(
            blank_node_property_list("[   a (),()\t]"),
            Ok((
                "",
                PropertyList(vec![VerbList::new(
                    Verb::A,
                    ObjectList(vec![
                        GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::Nil)),
                        GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::Nil)),
                    ]),
                )])
            ))
        );
    }
}
