use nom::{
    branch::alt,
    bytes::complete::take_while,
    bytes::complete::{escaped, tag, tag_no_case, take_while1, take_while_m_n},
    character::complete::{anychar, char, digit1, none_of, one_of},
    character::{
        complete::{alpha1, alphanumeric1},
        is_alphabetic,
    },
    combinator::{complete, cond, cut, map, map_res, not, opt, peek},
    error::ErrorKind,
    multi::{fold_many0, separated_list},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    AsChar, Err, IResult,
};

use crate::aggregate::count;
use crate::terminals::{preceded_tag1, prologue, sp, sp1};
use crate::{
    ask::ask_query,
    call::arg_list,
    clauses::values_clause,
    construct::construct_query,
    data::datablock,
    describe::describe_query,
    expression::{DefaultOrNamedIri, Iri, IriOrFunction, PrefixedName},
    graph::graph_term,
    node::{Collection, ObjectList, PropertyList, RdfLiteral, RdfLiteralDescriptor, TriplesNode},
    query::{BaseOrPrefixDecl, PrefixDecl, Prologue, SparqlQuery, SparqlQueryStatement},
    select::select_query,
    var::{Var, VarOrIri, VarOrTerm, VerbList},
};
use nom::character::{is_alphanumeric, is_digit};
use nom::combinator::recognize;
use nom::multi::{many0, many1};

pub fn sparql_query_stmt(i: &str) -> IResult<&str, SparqlQueryStatement> {
    map(
        tuple((
            terminated(prologue, sp),
            sparql_query,
            opt(preceded(sp1, preceded_tag1("values", datablock))),
        )),
        |(prologue, query, values)| SparqlQueryStatement {
            prologue,
            query,
            values,
        },
    )(i)
}

pub fn sparql_query(i: &str) -> IResult<&str, SparqlQuery> {
    alt((
        map(select_query, SparqlQuery::Select),
        map(construct_query, SparqlQuery::Construct),
        map(describe_query, SparqlQuery::Describe),
        map(ask_query, SparqlQuery::Ask),
    ))(i)
}

pub fn parse_query_bytes<T>(input: T) -> Result<SparqlQuery, &'static str>
where
    T: AsRef<[u8]>,
{
    unimplemented!()

    //    match sparql_query(input.as_ref()) {
    //        Ok((_, o)) => Ok(o),
    //        Err(_) => Err("failed to parse query"),
    //    }
}

pub fn parse_query<T>(input: T) -> Result<SparqlQuery, &'static str>
where
    T: AsRef<str>,
{
    parse_query_bytes(input.as_ref().trim().as_bytes())
}
