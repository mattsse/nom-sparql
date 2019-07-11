use nom::bytes::complete::tag_no_case;
use nom::character::complete::char;
use nom::combinator::map;
use nom::sequence::{delimited, separated_pair};
use nom::IResult;

use crate::arithmetic::{ConditionalOrExpression, NumericExpression};
use crate::expression::{ArgList, Expression, Iri};
use crate::literal::NumericLiteral;
use crate::node::RdfLiteral;
use crate::parser::{preceded_tag, sp_enc, var};
use crate::query::Var;

#[derive(Debug, Clone)]
pub enum Aggregate {
    Count(Count),
    Sum(AggregateModifier),
    Min(AggregateModifier),
    Max(AggregateModifier),
    Avg(AggregateModifier),
    Sample(AggregateModifier),
    GroupConcat(GroupConcat),
}

impl Aggregate {
    pub fn is_distinct(&self) -> bool {
        match self {
            Aggregate::Sum(a)
            | Aggregate::Min(a)
            | Aggregate::Max(a)
            | Aggregate::Avg(a)
            | Aggregate::Sample(a) => a.distinct,
            Aggregate::Count(c) => c.distinct,
            Aggregate::GroupConcat(c) => c.modifier.distinct,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Count {
    pub distinct: bool,
    pub target: CountTarget,
}

#[derive(Debug, Clone)]
pub enum CountTarget {
    All,
    Expr(Box<Expression>),
}

#[derive(Debug, Clone)]
pub struct GroupConcat {
    pub modifier: AggregateModifier,
    pub separator: Option<String>,
}

#[derive(Debug, Clone)]
pub struct AggregateModifier {
    pub distinct: bool,
    pub expression: Box<Expression>,
}
