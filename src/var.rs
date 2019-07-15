use nom::{
    branch::alt,
    bytes::complete::take_while,
    bytes::complete::{escaped, tag, tag_no_case, take_while1, take_while_m_n},
    character::complete::{anychar, char, digit1, none_of, one_of},
    character::{
        complete::{alpha1, alphanumeric1},
        is_alphabetic,
    },
    character::{is_alphanumeric, is_digit},
    combinator::recognize,
    combinator::{complete, cond, cut, map, map_res, not, opt, peek},
    error::ErrorKind,
    multi::{fold_many0, separated_list},
    multi::{many0, many1, separated_nonempty_list},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    AsChar, Err, IResult,
};

use crate::{
    aggregate::count,
    ask::ask_query,
    call::arg_list,
    clauses::values_clause,
    construct::construct_query,
    data::datablock,
    describe::describe_query,
    expression::{DefaultOrNamedIri, Iri, IriOrFunction, PrefixedName},
    graph::graph_term,
    graph::GraphTerm,
    node::{Collection, ObjectList, PropertyList, RdfLiteral, RdfLiteralDescriptor, TriplesNode},
    parser::{iri, is_pn_chars_u, sp},
    query::{BaseOrPrefixDecl, PrefixDecl, Prologue, SparqlQuery, SparqlQueryStatement},
    select::select_query,
};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum VarOrIri {
    Var(Var),
    Iri(Iri),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Var {
    QMark(String),
    Dollar(String),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum VarOrTerm {
    Var(Var),
    Term(GraphTerm),
}

#[derive(Debug, Clone, Eq, PartialEq, new)]
pub struct VerbList {
    pub verb: Verb,
    pub object_list: ObjectList,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum VarOrIrisOrAll {
    VarIri(Vec<VarOrIri>),
    All,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Verb {
    VarOrIri(VarOrIri),
    A,
}

pub(crate) fn var_or_iri(i: &str) -> IResult<&str, VarOrIri> {
    alt((map(var, VarOrIri::Var), map(iri, VarOrIri::Iri)))(i)
}

pub(crate) fn verb(i: &str) -> IResult<&str, Verb> {
    alt((
        map(var_or_iri, Verb::VarOrIri),
        map(tag_no_case("a"), |_| Verb::A),
    ))(i)
}

pub(crate) fn var_or_term(i: &str) -> IResult<&str, VarOrTerm> {
    alt((map(var, VarOrTerm::Var), map(graph_term, VarOrTerm::Term)))(i)
}

pub(crate) fn var(i: &str) -> IResult<&str, Var> {
    alt((
        map(
            preceded(char('?'), preceded(sp, map(var_name, str::to_string))),
            Var::QMark,
        ),
        map(
            preceded(char('$'), preceded(sp, map(var_name, str::to_string))),
            Var::Dollar,
        ),
    ))(i)
}

pub(crate) fn var_or_iris_or_all(i: &str) -> IResult<&str, VarOrIrisOrAll> {
    alt((
        map(
            separated_nonempty_list(sp, var_or_iri),
            VarOrIrisOrAll::VarIri,
        ),
        map(char('*'), |_| VarOrIrisOrAll::All),
    ))(i)
}

// TODO consider unicode cases in second
pub(crate) fn var_name(i: &str) -> IResult<&str, &str> {
    recognize(pair(
        take_while_m_n(1, 1, |c| is_pn_chars_u(c) || c.is_dec_digit()),
        take_while(|c| is_pn_chars_u(c) || c.is_dec_digit()),
    ))(i)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_verb() {
        assert_eq!(verb("a"), Ok(("", Verb::A)));

        assert_eq!(
            verb("?name"),
            Ok((
                "",
                Verb::VarOrIri(VarOrIri::Var(Var::QMark("name".to_string())))
            ))
        );

        assert_eq!(
            verb("<http://example.org/foaf/aliceFoaf>"),
            Ok((
                "",
                Verb::VarOrIri(VarOrIri::Iri(Iri::Iri(
                    "http://example.org/foaf/aliceFoaf".to_string()
                )))
            ))
        );
    }

    #[test]
    fn is_var_or_iri() {
        assert_eq!(
            var_or_iri("?name"),
            Ok(("", VarOrIri::Var(Var::QMark("name".to_string()))))
        );
    }

    #[test]
    fn is_var() {
        assert_eq!(var("?name"), Ok(("", Var::QMark("name".to_string()))));

        assert_eq!(var("$name"), Ok(("", Var::Dollar("name".to_string()))));
    }
}
