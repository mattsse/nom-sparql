use crate::node::RdfLiteral;
use crate::query::Var;

use crate::arithmetic::{ConditionalOrExpression, NumericExpression};
use crate::expression::{ArgList, Expression, Iri, RegexExpression};
use crate::literal::NumericLiteral;
use crate::parser::{preceded_tag, sp_enc, var};
use nom::bytes::complete::tag_no_case;
use nom::character::complete::char;
use nom::combinator::map;
use nom::sequence::{delimited, separated_pair};
use nom::IResult;

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub iri_ref: Iri,
    pub args: ArgList,
}

#[derive(Debug, Clone)]
pub enum BuiltInCall {
    Str(Box<Expression>),
    Lang(Box<Expression>),
    LangMatches {
        first: Box<Expression>,
        second: Box<Expression>,
    },
    Datatype(Box<Expression>),
    Bound(Var),
    SameTerm {
        first: Box<Expression>,
        second: Box<Expression>,
    },
    IsIri(Box<Expression>),
    IsUri(Box<Expression>),
    IsBlank(Box<Expression>),
    IsLiteral(Box<Expression>),
    Regex(Box<RegexExpression>),
}

pub(crate) fn function_call(i: &str) -> IResult<&str, FunctionCall> {
    unimplemented!()
}

pub(crate) fn built_in_call(i: &str) -> IResult<&str, BuiltInCall> {
    unimplemented!()
}
