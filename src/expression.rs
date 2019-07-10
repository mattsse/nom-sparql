use crate::node::RdfLiteral;
use crate::query::Var;

use crate::arithmetic::{ConditionalOrExpression, NumericExpression};
use crate::literal::NumericLiteral;
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
pub struct FunctionCall {
    pub iri_ref: Iri,
    pub args: ArgList,
}

#[derive(Debug, Clone)]
pub enum Constraint {
    Bracketted(Box<Expression>),
    BuiltInCall(BuiltInCall),
    FunctionCall(Box<FunctionCall>),
}

pub(crate) fn expression(_i: &str) -> IResult<&str, Expression> {
    unimplemented!()
}

pub(crate) fn function_call(i: &str) -> IResult<&str, FunctionCall> {
    unimplemented!()
}
