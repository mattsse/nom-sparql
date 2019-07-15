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

use crate::node::{Collection, ObjectList, PropertyList, TriplesNode, VarOrTerm, VerbList};
use crate::parser::{bracketted, sp, sp_enc, sp_sep, sp_sep1, var_or_iri, var_or_term};
use crate::path::{triples_same_subject_path, TriplesSameSubjectPath};
use crate::query::VarOrIri;
use crate::triple::{triples_template, TriplesTemplate};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Quads {
    pub first_triples: Option<TriplesTemplate>,
    pub entries: Vec<QuadsEntry>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct QuadsEntry {
    pub quads_not_triples: QuadsNotTriples,
    pub triples_template: Option<TriplesTemplate>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
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
