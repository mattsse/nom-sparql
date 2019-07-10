use crate::node::RdfLiteral;
use crate::query::Var;

use crate::arithmetic::{ConditionalOrExpression, NumericExpression};
use crate::call::FunctionCall;
use crate::literal::NumericLiteral;
use crate::parser::{preceded_tag, sp_enc, var};
use nom::bytes::complete::tag_no_case;
use nom::character::complete::char;
use nom::combinator::map;
use nom::sequence::{delimited, separated_pair};
use nom::IResult;

#[derive(Debug, Clone)]
pub struct Expression(ConditionalOrExpression);

#[derive(Debug, Clone)]
pub enum PrimaryExpression {
    BrackettedExpression(Box<Expression>),
    BuiltInCall(BuiltInCall),
    IriRefOrFunction(IriOrFunction),
    RdfLiteral(RdfLiteral),
    NumericLiteral,
    BooleanLiteral(bool),
    Var,
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

#[derive(Debug, Clone)]
pub struct RegexExpression {}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Iri {
    Iri(String),
    PrefixedName(PrefixedName),
}

impl Iri {
    pub fn iri_ref<T: ToString>(iri_ref: T) -> Self {
        Iri::Iri(iri_ref.to_string())
    }

    pub fn prefixed_name<T: Into<PrefixedName>>(prefixed_name: T) -> Self {
        Iri::PrefixedName(prefixed_name.into())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PrefixedName {
    PnameLN {
        pn_prefix: Option<String>,
        pn_local: String,
    },
    PnameNS(Option<String>),
}

#[derive(Debug, Clone)]
pub enum DefaultOrNamedIri {
    Default(Iri),
    Named(Iri),
}

#[derive(Debug, Clone)]
pub struct IriOrFunction {
    pub iri: Iri,
    pub arg_list: Option<ArgList>,
}

#[derive(Debug, Clone)]
pub enum Order {
    Asc,
    Desc,
}

#[derive(Debug, Clone)]
pub struct OrderExpression {
    pub order: Order,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub enum ArgList {
    Nil,
    Expression {
        first: Box<Expression>,
        further: Vec<Expression>,
    },
}

#[derive(Debug, Clone)]
pub enum Constraint {
    Bracketted(Box<Expression>),
    BuiltInCall(BuiltInCall),
    FunctionCall(Box<FunctionCall>),
}

#[derive(Debug, Clone)]
pub struct ExpressionAsVar {
    pub expression: Box<Expression>,
    pub var: Var,
}

pub(crate) fn expression_as_var(i: &str) -> IResult<&str, ExpressionAsVar> {
    delimited(
        char('('),
        sp_enc(map(
            separated_pair(expression, sp_enc(tag_no_case("as")), var),
            |(expression, var)| ExpressionAsVar {
                expression: Box::new(expression),
                var,
            },
        )),
        char(')'),
    )(i)
}

pub(crate) fn bind(i: &str) -> IResult<&str, ExpressionAsVar> {
    preceded_tag("bind", expression_as_var)(i)
}

#[derive(Debug, Clone)]
pub enum VarOrExpressionAsVar {
    Var(Var),
    ExpressionAsVar(ExpressionAsVar),
}

pub(crate) fn expression(_i: &str) -> IResult<&str, Expression> {
    unimplemented!()
}

pub(crate) fn function_call(i: &str) -> IResult<&str, FunctionCall> {
    unimplemented!()
}
