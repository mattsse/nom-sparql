use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case};
use nom::combinator::{map, opt};
use nom::sequence::{delimited, pair, preceded, terminated};
use nom::IResult;

use crate::expression::{Constraint, Iri};
use crate::graph::{GraphNode, GraphTerm};
use crate::literal::NumericLiteral;
use crate::parser::{iri, sp, sp1, sp_sep1};
use crate::query::{Var, VarOrIri};
use crate::triple::{property_list, property_list_not_empty, TriplesBlock, Verb};

#[derive(Debug, Clone)]
pub enum VarOrTerm {
    Var(Var),
    Term(GraphTerm),
}

#[derive(Debug, Clone)]
pub struct ObjectList(pub Vec<GraphNode>);

#[derive(Debug, Clone)]
pub struct VerbList {
    pub verb: Verb,
    pub object_list: ObjectList,
}

impl VerbList {
    pub fn new(verb: Verb, object_list: ObjectList) -> Self {
        VerbList { verb, object_list }
    }
}

#[derive(Debug, Clone)]
pub struct PropertyList(pub Vec<VerbList>);

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum BlankNode {
    Anon,
    Label(String),
}

#[derive(Debug, Clone)]
pub enum TriplesNode {
    Collection(Collection),
    BlankNodePropertyList(PropertyList),
}

#[derive(Debug, Clone)]
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
mod tests {}
