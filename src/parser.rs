use crate::ask::ask_query;
use crate::construct::construct_query;
use crate::data::datablock;
use crate::describe::describe_query;
use crate::query::{SparqlQuery, SparqlQueryStatement};
use crate::select::select_query;
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

pub fn parse_query_bytes<T>(_input: T) -> Result<SparqlQuery, &'static str>
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
