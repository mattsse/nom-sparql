use crate::expression::{Constraint, Iri};
use crate::literal::NumericLiteral;
use crate::parser::{iri, sp, sp1, sp_sep1};
use crate::query::{Var, VarOrIri};
use crate::triple::{property_list, property_list_not_empty, TriplesBlock, Verb};
use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case};
use nom::combinator::{map, opt};
use nom::sequence::{delimited, pair, preceded, terminated};
use nom::IResult;

#[derive(Debug, Clone)]
pub enum GraphOrDefault {
    Graph(Iri),
    Default,
}

#[derive(Debug, Clone)]
pub enum GraphRefAll {
    GraphRef(Iri),
    Default,
    Named,
    All,
}

#[derive(Debug, Clone)]
pub enum GraphNode {
    VarOrTerm(VarOrTerm),
    TriplesNode(Box<TriplesNode>),
}
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

#[derive(Debug, Clone)]
pub enum GraphTerm {
    IriRef(Iri),
    RdfLiteral(RdfLiteral),
    NumericLiteral(NumericLiteral),
    BooleanLiteral(bool),
    BlankNode(BlankNode),
    /// empty parentheses
    Nil,
}

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

#[derive(Debug, Clone)]
pub struct GroupGraphPattern {
    pub triples_block: Option<TriplesBlock>,
    pub pattern: Vec<GraphPattern>,
}

#[derive(Debug, Clone)]
pub struct GraphPattern {
    pub pattern_or_filter: GraphPatternOrFilter,
    pub triples_block: Option<TriplesBlock>,
}

#[derive(Debug, Clone)]
pub enum GraphPatternOrFilter {
    GraphPattern(Box<GraphPatternNotTriples>),
    Filter(Constraint),
}

#[derive(Debug, Clone)]
pub enum GraphPatternNotTriples {
    Optional(GroupGraphPattern),
    GroupOrUnion(GroupOrUnionGraphPattern),
    Graph(GraphGraphPattern),
}

#[derive(Debug, Clone)]
pub struct GroupOrUnionGraphPattern {
    pub first: GroupGraphPattern,
    pub unions: Vec<GroupGraphPattern>,
}

#[derive(Debug, Clone)]
pub struct GraphGraphPattern {
    pub var_or_iri_ref: VarOrIri,
    pub pattern: GroupGraphPattern,
}

pub(crate) fn graph_ref(i: &str) -> IResult<&str, Iri> {
    map(sp_sep1(tag_no_case("graph"), iri), |(_, iri)| iri)(i)
}

pub(crate) fn graph_or_default(i: &str) -> IResult<&str, GraphOrDefault> {
    alt((
        map(tag_no_case("default"), |_| GraphOrDefault::Default),
        map(
            pair(opt(pair(tag_no_case("graph"), sp1)), iri),
            |(_, iri)| GraphOrDefault::Graph(iri),
        ),
    ))(i)
}

pub(crate) fn graph_ref_all(i: &str) -> IResult<&str, GraphRefAll> {
    alt((
        map(tag_no_case("default"), |_| GraphRefAll::Default),
        map(tag_no_case("named"), |_| GraphRefAll::Named),
        map(tag_no_case("all"), |_| GraphRefAll::All),
        map(graph_ref, GraphRefAll::GraphRef),
    ))(i)
}

#[cfg(test)]
mod tests {}
