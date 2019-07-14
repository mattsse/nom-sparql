use std::str::FromStr;

use nom::character::complete::char;
use nom::combinator::{map, opt};
use nom::error::ErrorKind;
use nom::multi::{many0, many1, separated_nonempty_list};
use nom::sequence::{delimited, pair, separated_pair, tuple};
use nom::Err;
use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, tag_no_case, take_while1, take_while_m_n},
    character::is_digit,
    combinator::map_res,
    sequence::{preceded, terminated},
    IResult, Needed,
};

use crate::expression::Iri;
use crate::node::{ObjectList, VarOrTerm};
use crate::parser::{
    anon, iri, nil, pn_local, rdf_literal, sp, sp1, sp_enc, sp_sep, sp_sep1, var, var_or_iri,
    var_or_term,
};
use crate::query::Var;
use crate::triple::object_list;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum GraphNodePath {
    VarOrTerm(VarOrTerm),
    TriplesNodePath(TriplesNodePath),
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TriplesNodePath {
    CollectionPath(Vec<GraphNodePath>),
    BlankNodePropertyListPath(PropertyListPath),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TriplesSameSubjectPath {
    Term {
        var_or_term: VarOrTerm,
        property_list: PropertyListPath,
    },
    Node {
        triples_node: TriplesNodePath,
        property_list: Option<PropertyListPath>,
    },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PropertyListPath {
    pub verb_path_or_simple: VerbPathOrSimple,
    pub object_list_path: ObjectListPath,
    pub entries: Vec<PropertyListPathEntry>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PropertyListPathEntry {
    pub verb_path_or_simple: VerbPathOrSimple,
    pub object_list: ObjectList,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ObjectListPath(pub Vec<GraphNodePath>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum VerbPathOrSimple {
    Path(Path),
    Simple(Var),
}

pub(crate) type Path = PathAlternative;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PathAlternative(pub Vec<PathSequence>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PathSequence(pub Vec<PathEltOrInverse>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PathEltOrInverse {
    PathElt(PathElt),
    Inverse(PathElt),
}

#[derive(Debug, Clone, Eq, PartialEq)]
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PathPrimary {
    Iri(Iri),
    A,
    PathNegatedPropertySet(PathNegatedPropertySet),
    Path(Path),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PathNegatedPropertySet(pub Vec<PathOneInPropertySet>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PathOneInPropertySet(pub IriOrAOrCaret);

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

pub(crate) fn triples_same_subject_path(i: &str) -> IResult<&str, TriplesSameSubjectPath> {
    alt((
        map(
            sp_sep1(var_or_term, property_list_path_not_empty),
            |(var_or_term, property_list)| TriplesSameSubjectPath::Term {
                var_or_term,
                property_list,
            },
        ),
        map(
            sp_sep(triples_node_path, property_list_path),
            |(triples_node, property_list)| TriplesSameSubjectPath::Node {
                triples_node,
                property_list,
            },
        ),
    ))(i)
}

pub(crate) fn blank_node_property_list_path(i: &str) -> IResult<&str, PropertyListPath> {
    delimited(
        terminated(tag("["), sp),
        property_list_path_not_empty,
        preceded(sp, tag("]")),
    )(i)
}

pub(crate) fn property_list_path_not_empty(i: &str) -> IResult<&str, PropertyListPath> {
    map(
        tuple((
            terminated(verb_path_or_simple, sp1),
            object_list_path,
            many0(preceded(many1(sp_enc(char(';'))), property_list_path_entry)),
        )),
        |(verb_path_or_simple, object_list_path, entries)| PropertyListPath {
            verb_path_or_simple,
            object_list_path,
            entries,
        },
    )(i)
}

pub(crate) fn property_list_path(i: &str) -> IResult<&str, Option<PropertyListPath>> {
    opt(property_list_path_not_empty)(i)
}

pub(crate) fn verb_path_or_simple(i: &str) -> IResult<&str, VerbPathOrSimple> {
    alt((
        map(path_alternative, VerbPathOrSimple::Path),
        map(var, VerbPathOrSimple::Simple),
    ))(i)
}

pub(crate) fn object_list_path(i: &str) -> IResult<&str, ObjectListPath> {
    map(
        separated_nonempty_list(sp_enc(tag(";")), graph_node_path),
        ObjectListPath,
    )(i)
}

pub(crate) fn property_list_path_entry(i: &str) -> IResult<&str, PropertyListPathEntry> {
    map(
        separated_pair(verb_path_or_simple, sp1, object_list),
        |(verb_path_or_simple, object_list)| PropertyListPathEntry {
            verb_path_or_simple,
            object_list,
        },
    )(i)
}

pub(crate) fn triples_node_path(i: &str) -> IResult<&str, TriplesNodePath> {
    alt((
        map(
            blank_node_property_list_path,
            TriplesNodePath::BlankNodePropertyListPath,
        ),
        map(
            delimited(tag("("), many1(graph_node_path), tag(")")),
            TriplesNodePath::CollectionPath,
        ),
    ))(i)
}

pub(crate) fn graph_node_path(i: &str) -> IResult<&str, GraphNodePath> {
    alt((
        map(var_or_term, GraphNodePath::VarOrTerm),
        map(triples_node_path, GraphNodePath::TriplesNodePath),
    ))(i)
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
    if i.is_empty() {
        Err(Err::Incomplete(Needed::Size(1)))
    } else if let Ok(path) = PathMod::from_str(&i[0..1]) {
        Ok((&i[1..], path))
    } else {
        Err(Err::Error((i, ErrorKind::Char)))
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
