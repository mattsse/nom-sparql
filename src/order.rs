use nom::branch::alt;
use nom::bytes::complete::tag_no_case;
use nom::combinator::map;
use nom::sequence::separated_pair;
use nom::IResult;

use crate::expression::{bracketted_expression, constraint, Constraint, Expression};
use crate::parser::{sp1, var};
use crate::query::Var;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Order {
    Asc,
    Desc,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct OrderExpression {
    pub order: Order,
    pub expression: Expression,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum OrderCondition {
    Order(Box<OrderExpression>),
    Constraint(Box<Constraint>),
    Var(Var),
}

pub(crate) fn order(i: &str) -> IResult<&str, Order> {
    alt((
        map(tag_no_case("asc"), |_| Order::Asc),
        map(tag_no_case("desc"), |_| Order::Desc),
    ))(i)
}

pub(crate) fn order_expression(i: &str) -> IResult<&str, OrderExpression> {
    map(
        separated_pair(order, sp1, bracketted_expression),
        |(order, expression)| OrderExpression { order, expression },
    )(i)
}

pub(crate) fn order_condition(i: &str) -> IResult<&str, OrderCondition> {
    alt((
        map(order_expression, |order| {
            OrderCondition::Order(Box::new(order))
        }),
        map(constraint, |constraint| {
            OrderCondition::Constraint(Box::new(constraint))
        }),
        map(var, OrderCondition::Var),
    ))(i)
}
