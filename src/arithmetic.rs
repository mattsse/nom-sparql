use std::{fmt, str::FromStr};

use crate::expression::{expression_list, primary_expression, ExpressionList, PrimaryExpression};
use crate::literal::{numeric_literal, NumericLiteral};
use crate::parser::{preceded_tag, sp, sp1, sp_enc, sp_enc1};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::streaming::tag_no_case;
use nom::combinator::{map, opt};
use nom::multi::{separated_list, separated_nonempty_list};
use nom::sequence::{pair, preceded, terminated, tuple};
use nom::IResult;

#[derive(Debug, Clone)]
pub struct RelationalExpression {
    pub lhs: AdditiveExpression,
    pub rhs: Option<OpExpression>,
}

#[derive(Debug, Clone)]
pub struct ConditionalAndExpression(pub Vec<RelationalExpression>);

#[derive(Debug, Clone)]
pub enum OpExpression {
    EQ(AdditiveExpression),
    NE(AdditiveExpression),
    LT(AdditiveExpression),
    GT(AdditiveExpression),
    LE(AdditiveExpression),
    GE(AdditiveExpression),
    IN(ExpressionList),
    NIN(ExpressionList),
}

#[derive(Debug, Clone)]
pub struct AdditiveExpression {
    pub lhs: MultiplicativeExpression,
    pub rhs: Vec<AddExpression>,
}

#[derive(Debug, Clone)]
pub enum AddExpression {
    Add(MultiplicativeExpression),
    Sub(MultiplicativeExpression),
    Numeric {
        numeric: NumericLiteral,
        expressions: Vec<MultExpression>,
    },
}

#[derive(Debug, Clone)]
pub struct MultiplicativeExpression {
    pub lhs: UnaryExpression,
    pub rhs: Vec<MultExpression>,
}

#[derive(Debug, Clone)]
pub enum MultExpression {
    Mult(UnaryExpression),
    Div(UnaryExpression),
}

#[derive(Debug, Clone)]
pub enum UnaryExpression {
    Not(PrimaryExpression),
    Add(PrimaryExpression),
    Sub(PrimaryExpression),
    Default(PrimaryExpression),
}

#[derive(Debug, Clone, Eq, PartialEq, Copy)]
pub enum Sign {
    POS,
    NEG,
}

impl Default for Sign {
    fn default() -> Self {
        Sign::POS
    }
}

impl AsRef<str> for Sign {
    fn as_ref(&self) -> &str {
        match self {
            Sign::POS => "+",
            Sign::NEG => "-",
        }
    }
}

impl FromStr for Sign {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" | "" => Ok(Sign::POS),
            "-" => Ok(Sign::NEG),
            _ => Err(()),
        }
    }
}

impl fmt::Display for Sign {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.as_ref())
    }
}

pub(crate) fn unary_expression(i: &str) -> IResult<&str, UnaryExpression> {
    alt((
        map(preceded_tag("!", primary_expression), UnaryExpression::Not),
        map(preceded_tag("+", primary_expression), UnaryExpression::Add),
        map(preceded_tag("-", primary_expression), UnaryExpression::Sub),
        map(primary_expression, UnaryExpression::Default),
    ))(i)
}

pub(crate) fn additive_expression(i: &str) -> IResult<&str, AdditiveExpression> {
    map(
        pair(
            multiplicative_expression,
            preceded(sp, separated_list(sp, add_expression)),
        ),
        |(lhs, rhs)| AdditiveExpression { lhs, rhs },
    )(i)
}

pub(crate) fn add_expression(i: &str) -> IResult<&str, AddExpression> {
    alt((
        map(
            preceded_tag("+", multiplicative_expression),
            AddExpression::Add,
        ),
        map(
            preceded_tag("-", multiplicative_expression),
            AddExpression::Sub,
        ),
        map(
            pair(
                numeric_literal,
                preceded(sp, separated_list(sp, mult_expression)),
            ),
            |(numeric, expressions)| AddExpression::Numeric {
                numeric,
                expressions,
            },
        ),
    ))(i)
}

pub(crate) fn multiplicative_expression(i: &str) -> IResult<&str, MultiplicativeExpression> {
    map(
        pair(
            unary_expression,
            preceded(sp, separated_list(sp, mult_expression)),
        ),
        |(lhs, rhs)| MultiplicativeExpression { lhs, rhs },
    )(i)
}

pub(crate) fn mult_expression(i: &str) -> IResult<&str, MultExpression> {
    alt((
        map(preceded_tag("*", unary_expression), MultExpression::Mult),
        map(preceded_tag("/", unary_expression), MultExpression::Div),
    ))(i)
}

pub(crate) fn op_expression(i: &str) -> IResult<&str, OpExpression> {
    alt((
        map(preceded_tag("=", additive_expression), OpExpression::EQ),
        map(preceded_tag("!=", additive_expression), OpExpression::NE),
        map(preceded_tag("<", additive_expression), OpExpression::LT),
        map(preceded_tag(">", additive_expression), OpExpression::GT),
        map(preceded_tag("<=", additive_expression), OpExpression::LE),
        map(preceded_tag(">=", additive_expression), OpExpression::GE),
        map(preceded_tag("in", expression_list), OpExpression::IN),
        map(
            preceded(
                terminated(tag_no_case("not"), sp_enc1(tag_no_case("in"))),
                expression_list,
            ),
            OpExpression::NIN,
        ),
    ))(i)
}

pub(crate) fn relational_expression(i: &str) -> IResult<&str, RelationalExpression> {
    map(
        pair(additive_expression, opt(preceded(sp, op_expression))),
        |(lhs, rhs)| RelationalExpression { lhs, rhs },
    )(i)
}

pub(crate) fn conditional_and_expression(i: &str) -> IResult<&str, ConditionalAndExpression> {
    map(
        separated_nonempty_list(sp_enc(tag("&&")), relational_expression),
        ConditionalAndExpression,
    )(i)
}
