use nom::bytes::complete::tag_no_case;
use nom::character::complete::char;
use nom::combinator::{map, opt};
use nom::sequence::{delimited, pair, preceded, separated_pair};
use nom::IResult;

use crate::arithmetic::{ConditionalOrExpression, NumericExpression};
use crate::call::{BuiltInCall, FunctionCall};
use crate::literal::NumericLiteral;
use crate::node::RdfLiteral;
use crate::parser::{preceded_tag, sp_enc, sp_enc1, var};
use crate::query::Var;

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

#[derive(Debug, Clone)]
pub struct ExpressionAsVarOpt {
    pub expression: Box<Expression>,
    pub var: Option<Var>,
}

pub(crate) fn constraint(i: &str) -> IResult<&str, Constraint> {
    unimplemented!()
}

pub(crate) fn expression_as_var_opt(i: &str) -> IResult<&str, ExpressionAsVarOpt> {
    delimited(
        char('('),
        sp_enc(map(
            pair(expression, opt(preceded(sp_enc1(tag_no_case("as")), var))),
            |(expression, var)| ExpressionAsVarOpt {
                expression: Box::new(expression),
                var,
            },
        )),
        char(')'),
    )(i)
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

#[derive(Debug, Clone)]
pub enum VarOrExpressionAsVar {
    Var(Var),
    ExpressionAsVar(ExpressionAsVar),
}

pub(crate) fn expression(_i: &str) -> IResult<&str, Expression> {
    unimplemented!()
}

pub(crate) fn bracketted_expression(i: &str) -> IResult<&str, Expression> {
    delimited(char('('), sp_enc(expression), char(')'))(i)
}
