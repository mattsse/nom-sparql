use crate::parser::{
    anon, iri, nil, pn_local, rdf_literal, sp, sp1, sp_enc, sp_sep, sp_sep1, var_or_iri_ref,
    var_or_term,
};
use crate::query::VarOrIri;

use crate::expression::Iri;
use crate::triple::Verb;
use nom::bits::streaming::take;
use nom::character::complete::char;
use nom::combinator::{map, opt};
use nom::error::ErrorKind;
use nom::multi::separated_nonempty_list;
use nom::sequence::{delimited, pair};
use nom::Err;
use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, tag_no_case, take_while1, take_while_m_n},
    character::is_digit,
    combinator::map_res,
    sequence::{preceded, terminated},
    IResult, Needed,
};
use std::str::FromStr;

pub(crate) type Path = PathAlternative;

#[derive(Debug, Clone)]
pub struct PathAlternative(pub Vec<PathSequence>);

#[derive(Debug, Clone)]
pub struct PathSequence(pub Vec<PathEltOrInverse>);

#[derive(Debug, Clone)]
pub enum PathEltOrInverse {
    PathElt(PathElt),
    Inverse(PathElt),
}

#[derive(Debug, Clone)]
pub struct PathElt {
    pub path_primary: PathPrimary,
    pub path_mod: Option<PathMod>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PathMod {
    QuestionMark,
    Asterisk,
    Plus,
}

impl FromStr for PathMod {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "?" => Ok(PathMod::QuestionMark),
            "*" => Ok(PathMod::Asterisk),
            "+" => Ok(PathMod::Plus),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum PathPrimary {
    Iri(Iri),
    A,
    PathNegatedPropertySet(PathNegatedPropertySet),
    Path(Path),
}

#[derive(Debug, Clone)]
pub struct PathNegatedPropertySet(pub Vec<PathOneInPropertySet>);

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

pub(crate) fn path_alternative(i: &str) -> IResult<&str, PathAlternative> {
    map(
        separated_nonempty_list(delimited(sp, tag("|"), sp), path_sequence),
        PathAlternative,
    )(i)
}

pub(crate) fn path_sequence(i: &str) -> IResult<&str, PathSequence> {
    map(
        separated_nonempty_list(delimited(sp, tag("|"), sp), path_elt_or_inverse),
        PathSequence,
    )(i)
}

pub(crate) fn path_primary(i: &str) -> IResult<&str, PathPrimary> {
    alt((
        map(iri, PathPrimary::Iri),
        map(tag("a"), |_| PathPrimary::A),
        map(
            preceded(tag("!"), path_negated_property_set),
            PathPrimary::PathNegatedPropertySet,
        ),
        map(
            delimited(tag("("), path_alternative, tag(")")),
            PathPrimary::Path,
        ),
    ))(i)
}

pub(crate) fn path_elt_or_inverse(i: &str) -> IResult<&str, PathEltOrInverse> {
    alt((
        map(path_elt, PathEltOrInverse::PathElt),
        map(preceded(tag("^"), path_elt), PathEltOrInverse::Inverse),
    ))(i)
}

pub(crate) fn path_elt(i: &str) -> IResult<&str, PathElt> {
    map(
        pair(path_primary, opt(preceded(sp1, path_mod))),
        |(path_primary, path_mod)| PathElt {
            path_primary,
            path_mod,
        },
    )(i)
}

pub(crate) fn path_negated_property_set(i: &str) -> IResult<&str, PathNegatedPropertySet> {
    map(
        delimited(
            terminated(tag("("), sp),
            separated_nonempty_list(delimited(sp, char('|'), sp), path_one_in_property_set),
            preceded(sp, tag(")")),
        ),
        PathNegatedPropertySet,
    )(i)
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

pub(crate) fn path_mod(i: &str) -> IResult<&str, PathMod> {
    if i.len() == 0 {
        Err(Err::Incomplete(Needed::Size(1)))
    } else {
        if let Ok(path) = PathMod::from_str(&i[0..1]) {
            Ok((&i[1..], path))
        } else {
            Err(Err::Error((i, ErrorKind::Char)))
        }
    }
}

#[inline]
pub(crate) fn is_path_mod(c: char) -> bool {
    "?*+".contains(c)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_path_mode() {
        assert_eq!(path_mod("*"), Ok(("", PathMod::Asterisk)));
        assert_eq!(path_mod("+"), Ok(("", PathMod::Plus)));
        assert_eq!(path_mod("?"), Ok(("", PathMod::QuestionMark)));
    }

}
