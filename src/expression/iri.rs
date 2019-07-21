use crate::expression::Iri;
use crate::terminals::iri;
use nom::branch::alt;
use nom::bytes::complete::tag_no_case;
use nom::character::complete::char;
use nom::combinator::map;
use nom::sequence::preceded;
use nom::IResult;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum IriOrAOrCaret {
    IriOrA(IriOrA),
    /// [`IriOrA`] prefixed with a `^`
    Caret(IriOrA),
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum IriOrA {
    Iri(Iri),
    A,
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
