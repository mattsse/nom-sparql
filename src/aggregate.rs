use crate::{
    expression::{distinct_expression, expression, ArgList, DistinctExpression, Expression, Iri},
    literal::distinct,
    terminals::{bracketted, preceded_bracketted, preceded_tag, sp, sp1, sp_enc, string_literal},
};
use nom::{
    branch::alt,
    bytes::complete::tag_no_case,
    character::complete::char,
    combinator::{map, opt},
    sequence::{pair, preceded, terminated, tuple},
    IResult,
};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Aggregate {
    Count(Count),
    Sum(DistinctExpression),
    Min(DistinctExpression),
    Max(DistinctExpression),
    Avg(DistinctExpression),
    Sample(DistinctExpression),
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
            Aggregate::GroupConcat(c) => c.expression.distinct,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Count {
    pub distinct: bool,
    pub target: CountTarget,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum CountTarget {
    All,
    Expr(Box<Expression>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct GroupConcat {
    pub expression: DistinctExpression,
    pub separator: Option<String>,
}

pub(crate) fn aggregate(i: &str) -> IResult<&str, Aggregate> {
    alt((
        map(count, Aggregate::Count),
        map(
            preceded_bracketted("sum", distinct_expression),
            Aggregate::Sum,
        ),
        map(
            preceded_bracketted("min", distinct_expression),
            Aggregate::Min,
        ),
        map(
            preceded_bracketted("max", distinct_expression),
            Aggregate::Max,
        ),
        map(
            preceded_bracketted("avg", distinct_expression),
            Aggregate::Avg,
        ),
        map(
            preceded_bracketted("sample", distinct_expression),
            Aggregate::Sample,
        ),
        map(group_concat, Aggregate::GroupConcat),
    ))(i)
}

pub(crate) fn count_target(i: &str) -> IResult<&str, CountTarget> {
    alt((
        map(char('*'), |_| CountTarget::All),
        map(expression, |expr| CountTarget::Expr(Box::new(expr))),
    ))(i)
}

pub(crate) fn count(i: &str) -> IResult<&str, Count> {
    map(
        preceded_tag(
            "count",
            bracketted(pair(
                map(opt(terminated(distinct, sp1)), Option::unwrap_or_default),
                count_target,
            )),
        ),
        |(distinct, target)| Count { distinct, target },
    )(i)
}

pub(crate) fn group_concat(i: &str) -> IResult<&str, GroupConcat> {
    map(
        preceded_tag(
            "group_concat",
            bracketted(pair(
                distinct_expression,
                opt(preceded(
                    sp,
                    preceded(
                        tuple((
                            char(';'),
                            sp_enc(tag_no_case("separator")),
                            terminated(char('='), sp),
                        )),
                        map(string_literal, String::from),
                    ),
                )),
            )),
        ),
        |(expression, separator)| GroupConcat {
            expression,
            separator,
        },
    )(i)
}

#[cfg(test)]
mod tests {}
