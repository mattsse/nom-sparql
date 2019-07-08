use crate::expression::PrimaryExpression;
use crate::literal::NumericLiteral;
use std::{fmt, str::FromStr};

#[derive(Debug, Clone)]
pub struct RelationalExpression {
    pub lhs: NumericExpression,
    pub rhs: Option<NumericExpression>,
}

#[derive(Debug, Clone)]
pub struct ConditionalAndExpression {
    pub lhs: RelationalExpression,
    pub rhs: Vec<RelationalExpression>,
}

#[derive(Debug, Clone)]
pub struct ConditionalOrExpression {
    pub lhs: ConditionalAndExpression,
    pub rhs: Vec<ConditionalAndExpression>,
}

#[derive(Debug, Clone)]
pub enum NumericExpression {
    EQ(AdditiveExpression),
    NE(AdditiveExpression),
    LT(AdditiveExpression),
    GT(AdditiveExpression),
    LE(AdditiveExpression),
    GE(AdditiveExpression),
    Default(AdditiveExpression),
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
    NegNumericLiteral(NumericLiteral),
    PosNumericLiteral(NumericLiteral),
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
