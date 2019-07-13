use nom::IResult;

use crate::expression::{expression, ArgList, Expression, Iri, RegexExpression};

use crate::literal::distinct;
use crate::parser::{nil, sp, sp1, sp_enc};
use crate::query::Var;
use nom::branch::alt;
use nom::bytes::complete::tag_no_case;
use nom::character::complete::char;
use nom::combinator::{map, opt};
use nom::multi::separated_nonempty_list;
use nom::sequence::{delimited, terminated, tuple};

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

pub(crate) fn arg_list(i: &str) -> IResult<&str, ArgList> {
    alt((
        map(nil, |_| ArgList::Nil),
        map(
            delimited(
                terminated(char('('), sp),
                tuple((
                    map(opt(terminated(distinct, sp1)), Option::unwrap_or_default),
                    separated_nonempty_list(sp_enc(char(',')), expression),
                )),
                char(')'),
            ),
            |(distinct, expressions)| ArgList::Expression {
                distinct,
                expressions,
            },
        ),
    ))(i)
}
