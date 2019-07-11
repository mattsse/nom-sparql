use nom::character::complete::char;
use nom::combinator::{map, opt};
use nom::multi::{many0, many1, separated_nonempty_list};
use nom::sequence::{delimited, pair, separated_pair, tuple};
use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, tag_no_case, take_while1, take_while_m_n},
    character::is_digit,
    combinator::map_res,
    sequence::{preceded, terminated},
    IResult,
};

use crate::expression::ArgList;
use crate::graph::graph_node;

use crate::node::{
    Collection, ObjectList, PropertyList, TriplesNode, TriplesSameSubject, VerbList,
};
use crate::parser::{sp, sp_enc, sp_sep, sp_sep1, var_or_iri, var_or_term};
use crate::query::VarOrIri;

#[derive(Clone, Debug)]
pub enum Verb {
    VarOrIriRef(VarOrIri),
    A,
}

#[derive(Debug, Clone)]
pub struct ConstructTriples {
    pub first_triples: TriplesSameSubject,
    pub further_triples: Vec<TriplesSameSubject>,
}

#[derive(Debug, Clone)]
pub struct TriplesBlock {
    pub first_triples: TriplesSameSubject,
    pub further_triples: Vec<TriplesSameSubject>,
}

pub type TriplesTemplate = Vec<TriplesSameSubject>;

#[derive(Debug, Clone)]
pub struct Quads {
    pub first_triples: Option<TriplesTemplate>,
    pub entries: Vec<QuadsEntry>,
}

#[derive(Debug, Clone)]
pub struct QuadsEntry {
    pub quads_not_triples: QuadsNotTriples,
    pub triples_template: Option<TriplesTemplate>,
}

#[derive(Debug, Clone)]
pub struct QuadsNotTriples {
    pub var_or_iri: VarOrIri,
    pub triples_template: Option<TriplesTemplate>,
}

pub(crate) fn quads_pattern(i: &str) -> IResult<&str, Quads> {
    delimited(terminated(tag("{"), sp), quads, terminated(sp, tag("}")))(i)
}

pub(crate) fn quad_data(i: &str) -> IResult<&str, Quads> {
    quads_pattern(i)
}

pub(crate) fn quads(i: &str) -> IResult<&str, Quads> {
    map(
        pair(opt(terminated(triples_template, sp)), many0(quads_entry)),
        |(first_triples, entries)| Quads {
            first_triples,
            entries,
        },
    )(i)
}

pub(crate) fn quads_not_triples(i: &str) -> IResult<&str, QuadsNotTriples> {
    map(
        tuple((
            tag_no_case("graph"),
            sp_enc(var_or_iri),
            delimited(char('{'), sp_enc(opt(triples_template)), char('}')),
        )),
        |(_, var_or_iri, triples_template)| QuadsNotTriples {
            var_or_iri,
            triples_template,
        },
    )(i)
}

pub(crate) fn quads_entry(i: &str) -> IResult<&str, QuadsEntry> {
    map(
        separated_pair(quads_not_triples, opt(char('.')), opt(triples_template)),
        |(quads_not_triples, triples_template)| QuadsEntry {
            quads_not_triples,
            triples_template,
        },
    )(i)
}

pub(crate) fn arg_list(_i: &str) -> IResult<&str, ArgList> {
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
        map(var_or_iri, Verb::VarOrIriRef),
        map(tag_no_case("a"), |_| Verb::A),
    ))(i)
}

pub(crate) fn triples_node(i: &str) -> IResult<&str, TriplesNode> {
    alt((
        map(collection, TriplesNode::Collection),
        map(blank_node_property_list, TriplesNode::BlankNodePropertyList),
    ))(i)
}

pub(crate) fn triples_template(i: &str) -> IResult<&str, TriplesTemplate> {
    separated_nonempty_list(many1(sp_enc(char('.'))), triples_same_subject)(i)
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
