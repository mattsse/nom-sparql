use crate::parser::{
    anon, iri_ref, nil, pn_local, rdf_literal, sp_enc, sp_sep, sp_sep1, var_or_iri_ref, var_or_term,
};
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
use crate::node::{
    BlankNode, Collection, GraphNode, GraphTerm, ObjectList, PropertyList, TriplesNode,
    TriplesSameSubject, VerbList,
};

use crate::literal::{boolean, numeric_literal};
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

pub(crate) fn graph_term(i: &str) -> IResult<&str, GraphTerm> {
    alt((
        map(iri_ref, GraphTerm::IriRef),
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

pub(crate) fn object_list(i: &str) -> IResult<&str, ObjectList> {
    map(
        separated_nonempty_list(sp_enc(tag(",")), graph_node),
        ObjectList,
    )(i)
}

pub(crate) fn property_list_not_empty(i: &str) -> IResult<&str, PropertyList> {
    map(
        separated_nonempty_list(
            sp_enc(take_while1(|c| c == ';')),
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

pub(crate) fn triples_same_subject(i: &str) -> IResult<&str, TriplesSameSubject> {
    alt((
        map(
            sp_sep1(var_or_term, property_list_not_empty),
            |(var_or_term, property_list)| TriplesSameSubject::Term {
                var_or_term,
                property_list,
            },
        ),
        map(
            sp_sep(triples_node, property_list),
            |(triples_node, property_list)| TriplesSameSubject::Node {
                triples_node,
                property_list,
            },
        ),
    ))(i)
}

#[cfg(test)]
mod tests {

    #[test]
    fn is_object_list() {}
}
