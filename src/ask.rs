use crate::clauses::{solution_modifier, where_clause, SolutionModifier};
use crate::data::{data_set_clause, DataSetClause};
use crate::graph::GroupGraphPattern;
use crate::parser::{sp, sp1, sp_enc};
use nom::bytes::complete::tag_no_case;
use nom::combinator::map;
use nom::multi::separated_list;
use nom::sequence::{terminated, tuple};
use nom::IResult;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AskQuery {
    pub dataset_clauses: Vec<DataSetClause>,
    pub where_clause: GroupGraphPattern,
    pub solution_modifier: SolutionModifier,
}

pub fn ask_query(i: &str) -> IResult<&str, AskQuery> {
    map(
        tuple((
            terminated(tag_no_case("ask"), sp1),
            terminated(separated_list(sp, data_set_clause), sp),
            terminated(where_clause, sp),
            solution_modifier,
        )),
        |(_, dataset_clauses, where_clause, solution_modifier)| AskQuery {
            dataset_clauses,
            where_clause,
            solution_modifier,
        },
    )(i)
}
