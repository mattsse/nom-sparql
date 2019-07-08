use crate::parser::{
    anon, iri, nil, pn_local, rdf_literal, sp, sp_enc, sp_sep, sp_sep1, var_or_iri_ref, var_or_term,
};
use crate::query::VarOrIri;

use crate::expression::Iri;
use crate::triple::Verb;
use nom::character::complete::char;
use nom::combinator::map;
use nom::sequence::pair;
use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, tag_no_case, take_while1, take_while_m_n},
    character::is_digit,
    combinator::map_res,
    sequence::{preceded, terminated},
    IResult,
};

#[derive(Debug, Clone)]
pub struct PathNegatedPropertySet {}

#[derive(Debug, Clone)]
pub struct PathOneInPropertySet(pub IriOrAOrCaret);

#[derive(Debug, Clone)]
pub enum IriOrAOrCaret {
    IriOrA(IriOrA),
    /// [`IriOrA`] prefixed with a `^`
    Caret(IriOrA),
}
#[derive(Debug, Clone)]
pub enum IriOrA {
    Iri(Iri),
    A,
}

pub(crate) fn path_one_in_property_set(i: &str) -> IResult<&str, PathOneInPropertySet> {
    map(iri_or_a_or_caret, PathOneInPropertySet)(i)
}

pub(crate) fn iri_or_a(i: &str) -> IResult<&str, IriOrA> {
    alt((map(iri, IriOrA::Iri), map(tag_no_case("a"), |_| IriOrA::A)))(i)
}

pub(crate) fn iri_or_a_or_caret(i: &str) -> IResult<&str, IriOrAOrCaret> {
    alt((
        map(iri_or_a, IriOrAOrCaret::IriOrA),
        map(preceded(char('^'), iri_or_a), IriOrAOrCaret::Caret),
    ))(i)
}

#[inline]
pub(crate) fn is_path_mod(c: char) -> bool {
    "?*+".contains(c)
}
