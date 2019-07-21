use crate::data::datablock;
use crate::query::ask::ask_query;
use crate::query::construct::construct_query;
use crate::query::describe::describe_query;
use crate::query::select::select_query;
use crate::query::{SparqlQuery, SparqlQueryStatement};
use crate::terminals::{preceded_tag1, prologue, sp, sp1};
use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::sequence::{preceded, terminated, tuple};
use nom::IResult;

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
