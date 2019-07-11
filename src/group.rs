use crate::parser::{default_or_named_iri, iri, preceded_tag, sp, sp1, var};

use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, tag_no_case, take_while1, take_while_m_n},
    character::is_digit,
    combinator::map_res,
    sequence::{preceded, terminated},
    IResult,
};

use crate::call::{built_in_call, function_call, BuiltInCall, FunctionCall};
use crate::data::{datablock, DataBlock};
use crate::expression::{
    constraint, expression_as_var_opt, Constraint, DefaultOrNamedIri, ExpressionAsVar,
    ExpressionAsVarOpt, Iri,
};
use crate::graph::group_graph_pattern;
use crate::node::GroupGraphPattern;
use crate::query::{OrderCondition, Var};
use crate::triple::{quads_pattern, Quads};
use nom::combinator::{map, opt};
use nom::multi::separated_nonempty_list;
use nom::sequence::{delimited, separated_pair, tuple};

#[derive(Debug, Clone)]
pub struct GroupClause(pub Vec<GroupCondition>);

#[derive(Debug, Clone)]
pub enum GroupCondition {
    BuiltInCall(BuiltInCall),
    FunctionCall(FunctionCall),
    ExpressionAsVarOpt(ExpressionAsVarOpt),
    Var(Var),
}

pub(crate) fn group_clause(i: &str) -> IResult<&str, GroupClause> {
    map(
        preceded(
            terminated(delimited(tag_no_case("group"), sp1, tag_no_case("by")), sp1),
            separated_nonempty_list(sp, group_condition),
        ),
        GroupClause,
    )(i)
}

pub(crate) fn group_condition(i: &str) -> IResult<&str, GroupCondition> {
    alt((
        map(built_in_call, GroupCondition::BuiltInCall),
        map(function_call, GroupCondition::FunctionCall),
        map(expression_as_var_opt, GroupCondition::ExpressionAsVarOpt),
        map(var, GroupCondition::Var),
    ))(i)
}
