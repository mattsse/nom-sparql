use nom::bytes::complete::{tag, tag_no_case};
use nom::character::complete::char;
use nom::combinator::{map, opt};
use nom::sequence::{delimited, pair, preceded, separated_pair};
use nom::IResult;

use crate::arithmetic::{conditional_and_expression, ConditionalAndExpression};
use crate::call::{built_in_call, BuiltInCall, FunctionCall};

use crate::literal::{boolean, numeric_literal, NumericLiteral};
use crate::node::RdfLiteral;
use crate::parser::{iri_or_fun, nil, preceded_tag1, rdf_literal, sp_enc, sp_enc1, var};
use crate::query::Var;
use nom::branch::alt;
use nom::multi::separated_nonempty_list;

#[derive(Debug, Clone)]
pub struct Expression(pub Vec<ConditionalAndExpression>);

#[derive(Debug, Clone)]
pub enum ExpressionList {
    Nil,
    List(Vec<Expression>),
}

#[derive(Debug, Clone)]
pub enum PrimaryExpression {
    BrackettedExpression(Box<Expression>),
    BuiltInCall(BuiltInCall),
    IriOrFunction(IriOrFunction),
    RdfLiteral(RdfLiteral),
    NumericLiteral(NumericLiteral),
    BooleanLiteral(bool),
    Var(Var),
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
        distinct: bool,
        expressions: Vec<Expression>,
    },
}

#[derive(Debug, Clone)]
pub struct DistinctExpression {
    pub distinct: bool,
    pub expression: Expression,
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

#[derive(Debug, Clone)]
pub enum VarOrExpressionAsVar {
    Var(Var),
    ExpressionAsVar(ExpressionAsVar),
}

pub(crate) fn constraint(_i: &str) -> IResult<&str, Constraint> {
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

pub(crate) fn primary_expression(i: &str) -> IResult<&str, PrimaryExpression> {
    alt((
        map(bracketted_expression, |expr| {
            PrimaryExpression::BrackettedExpression(Box::new(expr))
        }),
        map(built_in_call, PrimaryExpression::BuiltInCall),
        map(iri_or_fun, PrimaryExpression::IriOrFunction),
        map(rdf_literal, PrimaryExpression::RdfLiteral),
        map(numeric_literal, PrimaryExpression::NumericLiteral),
        map(boolean, PrimaryExpression::BooleanLiteral),
        map(var, PrimaryExpression::Var),
    ))(i)
}

pub(crate) fn expression_list(i: &str) -> IResult<&str, ExpressionList> {
    alt((
        map(nil, |_| ExpressionList::Nil),
        map(
            delimited(
                char('('),
                separated_nonempty_list(sp_enc(char(',')), expression),
                char(')'),
            ),
            ExpressionList::List,
        ),
    ))(i)
}

pub(crate) fn bracketted_expression(i: &str) -> IResult<&str, Expression> {
    delimited(char('('), sp_enc(expression), char(')'))(i)
}

pub(crate) fn bind(i: &str) -> IResult<&str, ExpressionAsVar> {
    preceded_tag1("bind", expression_as_var)(i)
}

pub(crate) fn expression(i: &str) -> IResult<&str, Expression> {
    map(
        separated_nonempty_list(sp_enc(tag("||")), conditional_and_expression),
        Expression,
    )(i)
}
