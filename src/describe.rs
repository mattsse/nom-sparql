use crate::clauses::{solution_modifier, where_clause, SolutionModifier};
use crate::data::{data_set_clause, DataSetClause};
use crate::graph::GroupGraphPattern;
use crate::parser::{sp, sp1, sp_enc, sp_enc1, var_or_iri};
use crate::query::VarOrIri;
use nom::{
    branch::alt,
    bytes::complete::tag_no_case,
    character::complete::char,
    combinator::{map, opt},
    multi::{separated_list, separated_nonempty_list},
    sequence::{terminated, tuple},
    IResult,
};

#[derive(Debug, Clone)]
pub struct DescribeQuery {
    pub dataset_clauses: Vec<DataSetClause>,
    pub var_or_iris_or_all: VarOrIrisOrAll,
    pub where_clause: Option<GroupGraphPattern>,
    pub solution_modifier: SolutionModifier,
}

#[derive(Debug, Clone)]
pub enum VarOrIrisOrAll {
    VarIri(Vec<VarOrIri>),
    All,
}

pub fn describe_query(i: &str) -> IResult<&str, DescribeQuery> {
    map(
        tuple((
            tag_no_case("describe"),
            sp_enc1(var_or_iris_or_all),
            terminated(separated_list(sp, data_set_clause), sp),
            opt(terminated(where_clause, sp)),
            solution_modifier,
        )),
        |(_, var_or_iris_or_all, dataset_clauses, where_clause, solution_modifier)| DescribeQuery {
            var_or_iris_or_all,
            dataset_clauses,
            where_clause,
            solution_modifier,
        },
    )(i)
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
