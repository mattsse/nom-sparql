use crate::clauses::{solution_modifier, where_clause, SolutionModifier};
use crate::data::{data_set_clause, DataSetClause};
use crate::graph::GroupGraphPattern;
use crate::terminals::{sp, sp_enc1};
use crate::var::{var_or_iri, var_or_iris_or_all, VarOrIri, VarOrIrisOrAll};
use nom::{
    branch::alt,
    bytes::complete::tag_no_case,
    character::complete::char,
    combinator::{map, opt},
    multi::{separated_list, separated_nonempty_list},
    sequence::{terminated, tuple},
    IResult,
};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DescribeQuery {
    pub dataset_clauses: Vec<DataSetClause>,
    pub var_or_iris_or_all: VarOrIrisOrAll,
    pub where_clause: Option<GroupGraphPattern>,
    pub solution_modifier: SolutionModifier,
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
