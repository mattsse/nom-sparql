use crate::query::SparqlQuery;
use nom::IResult;

fn sparql_query(i: &[u8]) -> IResult<&[u8], SparqlQuery> {
    unimplemented!()
}

pub fn parse_query_bytes<T>(input: T) -> Result<SparqlQuery, &'static str>
where
    T: AsRef<[u8]>,
{
    match sparql_query(input.as_ref()) {
        Ok((_, o)) => Ok(o),
        Err(_) => Err("failed to parse query"),
    }
}

pub fn parse_query<T>(input: T) -> Result<SparqlQuery, &'static str>
where
    T: AsRef<str>,
{
    parse_query_bytes(input.as_ref().trim().as_bytes())
}
