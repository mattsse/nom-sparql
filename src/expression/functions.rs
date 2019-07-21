use crate::call::{built_in_call, function_call, BuiltInCall, FunctionCall};
use crate::expression::{bracketted_expr3, bracketted_expression, expression, Expression};
use crate::literal::StringLiteral;
use crate::terminals::{bracketted, preceded_tag1, sp_enc};
use nom::branch::alt;
use nom::character::complete::char;
use nom::combinator::{map, opt};
use nom::sequence::{preceded, tuple};
use nom::IResult;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RegexExpression {
    pub first: Expression,
    pub second: Expression,
    pub third: Option<Expression>,
}

pub type SubstringExpression = RegexExpression;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StrReplaceExpression {
    pub first: Expression,
    pub second: Expression,
    pub third: Expression,
    pub fourth: Option<Expression>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Constraint {
    Bracketted(Expression),
    BuiltInCall(BuiltInCall),
    FunctionCall(FunctionCall),
}

pub(crate) fn regex_expression(i: &str) -> IResult<&str, RegexExpression> {
    map(
        preceded_tag1("regex", bracketted_expr3),
        |(first, second, third)| RegexExpression {
            first,
            second,
            third,
        },
    )(i)
}

pub(crate) fn substring_expression(i: &str) -> IResult<&str, SubstringExpression> {
    map(
        preceded_tag1("substr", bracketted_expr3),
        |(first, second, third)| SubstringExpression {
            first,
            second,
            third,
        },
    )(i)
}

pub(crate) fn str_replace_expression(i: &str) -> IResult<&str, StrReplaceExpression> {
    map(
        bracketted(tuple((
            expression,
            preceded(sp_enc(char(',')), expression),
            preceded(sp_enc(char(',')), expression),
            opt(preceded(sp_enc(char(',')), expression)),
        ))),
        |(first, second, third, fourth)| StrReplaceExpression {
            first,
            second,
            third,
            fourth,
        },
    )(i)
}

pub(crate) fn constraint(i: &str) -> IResult<&str, Constraint> {
    alt((
        map(bracketted_expression, Constraint::Bracketted),
        map(built_in_call, Constraint::BuiltInCall),
        map(function_call, Constraint::FunctionCall),
    ))(i)
}

pub(crate) fn filter(i: &str) -> IResult<&str, Constraint> {
    preceded_tag1("filter", constraint)(i)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_filter() {}
}
