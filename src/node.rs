use crate::expression::Iri;
use crate::graph::{GraphNode, GraphTerm};

use crate::query::Var;
use crate::triple::Verb;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum VarOrTerm {
    Var(Var),
    Term(GraphTerm),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ObjectList(pub Vec<GraphNode>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VerbList {
    pub verb: Verb,
    pub object_list: ObjectList,
}

impl VerbList {
    pub fn new(verb: Verb, object_list: ObjectList) -> Self {
        VerbList { verb, object_list }
    }
}

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

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TriplesSameSubject {
    Term {
        var_or_term: VarOrTerm,
        property_list: PropertyList,
    },
    Node {
        triples_node: TriplesNode,
        property_list: Option<PropertyList>,
    },
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::blank_node;
    use crate::triple::collection;

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
                Collection(vec![GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::Nil)),
                                GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::Nil))

                ])
            ))
        );
    }

}
