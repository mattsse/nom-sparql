use nom::IResult;

use crate::expression::{ArgList, Expression, Iri, RegexExpression};

use crate::query::Var;

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

pub(crate) fn function_call(_i: &str) -> IResult<&str, FunctionCall> {
    unimplemented!()
}

pub(crate) fn built_in_call(_i: &str) -> IResult<&str, BuiltInCall> {
    unimplemented!()
}
