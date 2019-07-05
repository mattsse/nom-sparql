use crate::parser::{object_list, sep, var_or_iri_ref};
use crate::query::VarOrIriRef;

use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, tag_no_case, take_while1, take_while_m_n},
    character::is_digit,
    combinator::map_res,
    sequence::{preceded, terminated},
    IResult,
};

use crate::expression::ArgList;
use crate::node::{Collection, GraphNode, GraphTerm, PropertyList, TriplesNode, VerbList};

use nom::combinator::{map, opt};
use nom::multi::{many1, separated_nonempty_list};
use nom::sequence::{delimited, pair};

#[derive(Clone, Debug)]
pub enum Verb {
    VarOrIriRef(VarOrIriRef),
    A,
}

pub(crate) fn arg_list(_i: &str) -> IResult<&str, ArgList> {
    unimplemented!()
}

pub(crate) fn graph_term(_i: &str) -> IResult<&str, GraphTerm> {
    unimplemented!()
}
pub(crate) fn graph_node(_i: &str) -> IResult<&str, GraphNode> {
    unimplemented!()
}

#[inline]
pub(crate) fn blank_node_property_list(i: &str) -> IResult<&str, PropertyList> {
    delimited(tag("["), property_list_not_empty, tag("["))(i)
}

pub(crate) fn collection(i: &str) -> IResult<&str, Collection> {
    map(
        delimited(tag("()"), many1(graph_node), tag("()")),
        Collection,
    )(i)
}

pub(crate) fn property_list_not_empty(i: &str) -> IResult<&str, PropertyList> {
    map(
        separated_nonempty_list(
            sep(take_while1(|c| c == ';')),
            map(pair(verb, object_list), |(v, l)| VerbList::new(v, l)),
        ),
        PropertyList,
    )(i)
}

#[inline]
pub(crate) fn property_list(i: &str) -> IResult<&str, Option<PropertyList>> {
    opt(property_list_not_empty)(i)
}

pub(crate) fn verb(i: &str) -> IResult<&str, Verb> {
    alt((
        map(var_or_iri_ref, Verb::VarOrIriRef),
        map(tag_no_case("a"), |_| Verb::A),
    ))(i)
}

pub(crate) fn triples_node(i: &str) -> IResult<&str, TriplesNode> {
    alt((
        map(collection, TriplesNode::Collection),
        map(blank_node_property_list, TriplesNode::BlankNodePropertyList),
    ))(i)
}
