use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, tag_no_case, take_while1, take_while_m_n},
    character::is_digit,
    combinator::map_res,
    combinator::{map, opt},
    sequence::separated_pair,
    sequence::{delimited, pair, preceded, terminated},
    IResult,
};

use crate::literal::{boolean, numeric_literal};
use crate::node::{BlankNode, RdfLiteral, TriplesNode, VarOrTerm};
use crate::parser::{anon, nil, pn_local, rdf_literal, var_or_term};
use crate::triple::triples_node;
use crate::{
    data::{datablock, DataBlock},
    expression::DefaultOrNamedIri,
    expression::{Constraint, Iri},
    literal::NumericLiteral,
    parser::sp_sep1,
    parser::{default_or_named_iri, iri, preceded_tag, sp, sp1},
    query::{Var, VarOrIri},
    triple::{property_list, property_list_not_empty, TriplesBlock, Verb},
    triple::{quads_pattern, Quads},
};

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

#[derive(Debug, Clone)]
pub struct GroupGraphPatternSub {}

pub(crate) fn group_graph_pattern(i: &str) -> IResult<&str, GroupGraphPattern> {
    unimplemented!()
}

pub(crate) fn graph_term(i: &str) -> IResult<&str, GraphTerm> {
    alt((
        map(iri, GraphTerm::IriRef),
        map(rdf_literal, GraphTerm::RdfLiteral),
        map(numeric_literal, GraphTerm::NumericLiteral),
        map(boolean, GraphTerm::BooleanLiteral),
        map(blank_node, GraphTerm::BlankNode),
        map(nil, |_| GraphTerm::Nil),
    ))(i)
}

pub(crate) fn blank_node(i: &str) -> IResult<&str, BlankNode> {
    alt((
        map(sp_sep1(tag("_:"), pn_local), |(_, label)| {
            BlankNode::Label(label)
        }),
        map(anon, |_| BlankNode::Anon),
    ))(i)
}

pub(crate) fn graph_node(i: &str) -> IResult<&str, GraphNode> {
    alt((
        map(var_or_term, GraphNode::VarOrTerm),
        map(triples_node, |node| GraphNode::TriplesNode(Box::new(node))),
    ))(i)
}
