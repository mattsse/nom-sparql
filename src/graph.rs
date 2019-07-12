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

use crate::data::inline_data;
use crate::expression::{bind, constraint, ExpressionAsVar};
use crate::literal::{boolean, numeric_literal, silent};
use crate::node::{BlankNode, RdfLiteral, TriplesNode, VarOrTerm};
use crate::parser::{anon, nil, pn_local, rdf_literal, sp_enc, sp_enc1, var_or_iri, var_or_term};
use crate::select::{sub_select, SubSelect};
use crate::triple::{triples_block, triples_node};
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
use nom::character::complete::char;
use nom::multi::{many0, separated_list, separated_nonempty_list};
use nom::sequence::tuple;

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
pub struct GroupGraphPatternSub {
    pub triples_block: Option<TriplesBlock>,
    pub graph_pattern_and_triples: Vec<GraphPatternAndTriples>,
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
pub enum GroupGraphPattern {
    SubSelect(Box<SubSelect>),
    GroupGraphPatternSub(GroupGraphPatternSub),
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
    Minus(GroupGraphPattern),
    Graph(GraphGraphPattern),
    Service(ServiceGraphPattern),
    Filter(Constraint),
    Bind(ExpressionAsVar),
    InlineData(DataBlock),
}

#[derive(Debug, Clone)]
pub struct GraphPatternAndTriples {
    pub graph_pattern: GraphPatternNotTriples,
    pub triples: Option<TriplesBlock>,
}

#[derive(Debug, Clone)]
pub struct GroupOrUnionGraphPattern(pub Vec<GroupGraphPattern>);

#[derive(Debug, Clone)]
pub struct GraphGraphPattern {
    pub var_or_iri: VarOrIri,
    pub graph_pattern: GroupGraphPattern,
}

#[derive(Debug, Clone)]
pub struct ServiceGraphPattern {
    pub var_or_iri: VarOrIri,
    pub silent: bool,
    pub graph_pattern: GroupGraphPattern,
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

pub(crate) fn graph_pattern_not_triples(i: &str) -> IResult<&str, GraphPatternNotTriples> {
    alt((
        map(
            group_or_union_graph_pattern,
            GraphPatternNotTriples::GroupOrUnion,
        ),
        map(
            optional_group_graph_pattern,
            GraphPatternNotTriples::Optional,
        ),
        map(minus_graph_pattern, GraphPatternNotTriples::Minus),
        map(graph_graph_pattern, GraphPatternNotTriples::Graph),
        map(service_graph_pattern, GraphPatternNotTriples::Service),
        map(filter, GraphPatternNotTriples::Filter),
        map(bind, GraphPatternNotTriples::Bind),
        map(inline_data, GraphPatternNotTriples::InlineData),
    ))(i)
}

pub(crate) fn group_or_union_graph_pattern(i: &str) -> IResult<&str, GroupOrUnionGraphPattern> {
    map(
        separated_nonempty_list(sp_enc1(tag_no_case("union")), group_graph_pattern),
        GroupOrUnionGraphPattern,
    )(i)
}
pub(crate) fn optional_group_graph_pattern(i: &str) -> IResult<&str, GroupGraphPattern> {
    preceded_tag("optional", group_graph_pattern)(i)
}
pub(crate) fn minus_graph_pattern(i: &str) -> IResult<&str, GroupGraphPattern> {
    preceded_tag("minus", group_graph_pattern)(i)
}

pub(crate) fn graph_graph_pattern(i: &str) -> IResult<&str, GraphGraphPattern> {
    map(
        tuple((
            terminated(tag_no_case("graph"), sp1),
            terminated(var_or_iri, sp),
            group_graph_pattern,
        )),
        |(_, var_or_iri, graph_pattern)| GraphGraphPattern {
            var_or_iri,
            graph_pattern,
        },
    )(i)
}

pub(crate) fn group_graph_pattern_sub(i: &str) -> IResult<&str, GroupGraphPatternSub> {
    map(
        pair(
            opt(terminated(triples_block, sp)),
            separated_list(sp, group_graph_pattern_and_triples),
        ),
        |(triples_block, graph_pattern_and_triples)| GroupGraphPatternSub {
            triples_block,
            graph_pattern_and_triples,
        },
    )(i)
}

pub(crate) fn group_graph_pattern_and_triples(i: &str) -> IResult<&str, GraphPatternAndTriples> {
    map(
        separated_pair(
            graph_pattern_not_triples,
            opt(preceded(sp, char('?'))),
            opt(preceded(sp, triples_block)),
        ),
        |(graph_pattern, triples)| GraphPatternAndTriples {
            graph_pattern,
            triples,
        },
    )(i)
}

pub(crate) fn service_graph_pattern(i: &str) -> IResult<&str, ServiceGraphPattern> {
    map(
        tuple((
            terminated(tag_no_case("service"), sp1),
            map(opt(terminated(silent, sp1)), Option::unwrap_or_default),
            terminated(var_or_iri, sp),
            group_graph_pattern,
        )),
        |(_, silent, var_or_iri, graph_pattern)| ServiceGraphPattern {
            var_or_iri,
            silent,
            graph_pattern,
        },
    )(i)
}

pub(crate) fn filter(i: &str) -> IResult<&str, Constraint> {
    preceded_tag("filter", constraint)(i)
}

pub(crate) fn group_graph_pattern(i: &str) -> IResult<&str, GroupGraphPattern> {
    delimited(
        char('{'),
        sp_enc(alt((
            map(sub_select, |s| GroupGraphPattern::SubSelect(Box::new(s))),
            map(
                group_graph_pattern_sub,
                GroupGraphPattern::GroupGraphPatternSub,
            ),
        ))),
        char('}'),
    )(i)
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
