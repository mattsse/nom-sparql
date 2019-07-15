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

use crate::{
    data::inline_data,
    data::{datablock, DataBlock},
    expression::DefaultOrNamedIri,
    expression::{bind, constraint, ExpressionAsVar},
    expression::{Constraint, Iri},
    literal::NumericLiteral,
    literal::{boolean, numeric_literal, silent},
    node::{BlankNode, RdfLiteral, TriplesNode},
    quads::{quads_pattern, Quads},
    select::{sub_select, SubSelect},
    terminals::sp_sep1,
    terminals::{
        anon, nil, pn_chars_tail, pn_chars_u_one, pn_local, rdf_literal, sp_enc, sp_enc1, sp_sep,
    },
    terminals::{default_or_named_iri, iri, preceded_tag1, sp, sp1},
    triple::{property_list, property_list_not_empty, TriplesBlock},
    triple::{triples_block, triples_node},
    var::{var_or_iri, var_or_term, Var, VarOrIri, VarOrTerm, Verb},
};
use nom::character::complete::char;
use nom::combinator::recognize;
use nom::multi::{separated_list, separated_nonempty_list};
use nom::sequence::tuple;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum GraphTerm {
    Iri(Iri),
    RdfLiteral(RdfLiteral),
    NumericLiteral(NumericLiteral),
    BooleanLiteral(bool),
    BlankNode(BlankNode),
    /// empty parentheses
    Nil,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct GroupGraphPatternSub {
    pub triples_block: Option<TriplesBlock>,
    pub graph_pattern_and_triples: Vec<GraphPatternAndTriples>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum GraphOrDefault {
    Graph(Iri),
    Default,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum GraphRefAll {
    GraphRef(Iri),
    Default,
    Named,
    All,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum GraphNode {
    VarOrTerm(VarOrTerm),
    TriplesNode(Box<TriplesNode>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum GroupGraphPattern {
    SubSelect(Box<SubSelect>),
    GroupGraphPatternSub(GroupGraphPatternSub),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct GraphPattern {
    pub pattern_or_filter: GraphPatternOrFilter,
    pub triples_block: Option<TriplesBlock>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum GraphPatternOrFilter {
    GraphPattern(Box<GraphPatternNotTriples>),
    Filter(Constraint),
}

#[derive(Debug, Clone, Eq, PartialEq)]
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct GraphPatternAndTriples {
    pub graph_pattern: GraphPatternNotTriples,
    pub triples: Option<TriplesBlock>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct GroupOrUnionGraphPattern(pub Vec<GroupGraphPattern>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct GraphGraphPattern {
    pub var_or_iri: VarOrIri,
    pub graph_pattern: GroupGraphPattern,
}

#[derive(Debug, Clone, Eq, PartialEq)]
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
    preceded_tag1("optional", group_graph_pattern)(i)
}
pub(crate) fn minus_graph_pattern(i: &str) -> IResult<&str, GroupGraphPattern> {
    preceded_tag1("minus", group_graph_pattern)(i)
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
    preceded_tag1("filter", constraint)(i)
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
        map(iri, GraphTerm::Iri),
        map(rdf_literal, GraphTerm::RdfLiteral),
        map(numeric_literal, GraphTerm::NumericLiteral),
        map(boolean, GraphTerm::BooleanLiteral),
        map(blank_node, GraphTerm::BlankNode),
        map(nil, |_| GraphTerm::Nil),
    ))(i)
}

pub(crate) fn blank_node(i: &str) -> IResult<&str, BlankNode> {
    alt((
        map(
            sp_sep(
                tag("_:"),
                recognize(pair(
                    alt((pn_chars_u_one, take_while_m_n(1, 1, |c| is_digit(c as u8)))),
                    pn_chars_tail,
                )),
            ),
            |(_, label)| BlankNode::Label(label.to_string()),
        ),
        map(anon, |_| BlankNode::Anon),
    ))(i)
}

pub(crate) fn graph_node(i: &str) -> IResult<&str, GraphNode> {
    alt((
        map(var_or_term, GraphNode::VarOrTerm),
        map(triples_node, |node| GraphNode::TriplesNode(Box::new(node))),
    ))(i)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expression::PrefixedName;

    #[test]
    fn is_graph_ref_all() {
        assert_eq!(graph_ref_all("default"), Ok(("", GraphRefAll::Default)));
        assert_eq!(
            graph_ref_all("graph :uri1"),
            Ok((
                "",
                GraphRefAll::GraphRef(Iri::PrefixedName(PrefixedName::PnameLN {
                    pn_prefix: None,
                    pn_local: "uri1".to_string(),
                },))
            ))
        );
        assert_eq!(
            graph_ref_all("graph <http://example.org/foaf/aliceFoaf>"),
            Ok((
                "",
                GraphRefAll::GraphRef(Iri::Iri("http://example.org/foaf/aliceFoaf".to_string()))
            ))
        );
    }

    #[test]
    fn is_graph_term() {
        assert_eq!(graph_term("()"), Ok(("", GraphTerm::Nil)));
        assert_eq!(
            graph_term("true"),
            Ok(("", GraphTerm::BooleanLiteral(true)))
        );
        assert_eq!(
            graph_term("<http://example.org/foaf/aliceFoaf>"),
            Ok((
                "",
                GraphTerm::Iri(Iri::Iri("http://example.org/foaf/aliceFoaf".to_string()))
            ))
        );

        assert_eq!(
            graph_term("-5"),
            Ok(("", GraphTerm::NumericLiteral(NumericLiteral::Int(-5))))
        );
    }

    #[test]
    fn is_var_or_term() {
        assert_eq!(var_or_term("()"), Ok(("", VarOrTerm::Term(GraphTerm::Nil))));
        assert_eq!(
            var_or_term("false"),
            Ok(("", VarOrTerm::Term(GraphTerm::BooleanLiteral(false))))
        );

        assert_eq!(
            var_or_term("?name"),
            Ok(("", VarOrTerm::Var(Var::QMark("name".to_string()))))
        );
    }

}
