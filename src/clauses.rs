use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, tag_no_case, take_while1, take_while_m_n},
    character::is_digit,
    combinator::map_res,
    combinator::{map, opt},
    multi::separated_nonempty_list,
    sequence::{delimited, separated_pair, tuple},
    sequence::{preceded, terminated},
    IResult,
};

use crate::{
    data::{datablock, DataBlock},
    expression::{constraint, Constraint, DefaultOrNamedIri},
    graph::group_graph_pattern,
    graph::GroupGraphPattern,
    group::{group_clause, GroupClause},
    order::{order_condition, OrderCondition},
    parser::{default_or_named_iri, preceded_tag1, sp, sp1},
    triple::{quads_pattern, Quads},
};

#[derive(Debug, Clone)]
pub struct SolutionModifier {
    pub order_by: Option<OrderClause>,
    pub group_by: Option<GroupClause>,
    pub having: Option<HavingClause>,
    pub limit_offset: Option<LimitOffsetClause>,
}

#[derive(Debug, Clone)]
pub struct HavingClause(pub Vec<Constraint>);

#[derive(Debug, Clone)]
pub struct OrderClause(pub Vec<OrderCondition>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum LimitOffsetClause {
    LimitOffset { limit: u64, offset: Option<u64> },
    OffsetLimit { offset: u64, limit: Option<u64> },
}

impl LimitOffsetClause {
    pub fn limit_offset(limit: u64, offset: Option<u64>) -> Self {
        LimitOffsetClause::LimitOffset { limit, offset }
    }
    pub fn offset_limit(offset: u64, limit: Option<u64>) -> Self {
        LimitOffsetClause::OffsetLimit { offset, limit }
    }
}

pub(crate) fn solution_modifier(i: &str) -> IResult<&str, SolutionModifier> {
    map(
        tuple((
            opt(group_clause),
            opt(preceded(sp, having_clause)),
            opt(preceded(sp, order_clause)),
            opt(preceded(sp, limit_offset_clause)),
        )),
        |(group_by, having, order_by, limit_offset)| SolutionModifier {
            group_by,
            having,
            order_by,
            limit_offset,
        },
    )(i)
}

pub(crate) fn order_clause(i: &str) -> IResult<&str, OrderClause> {
    map(
        preceded(
            terminated(delimited(tag_no_case("order"), sp1, tag_no_case("by")), sp1),
            separated_nonempty_list(sp, order_condition),
        ),
        OrderClause,
    )(i)
}

pub(crate) fn having_clause(i: &str) -> IResult<&str, HavingClause> {
    map(
        preceded(
            terminated(tag_no_case("having"), sp1),
            separated_nonempty_list(sp, constraint),
        ),
        HavingClause,
    )(i)
}

pub(crate) fn where_clause(i: &str) -> IResult<&str, GroupGraphPattern> {
    preceded(
        opt(terminated(tag_no_case("where"), sp1)),
        group_graph_pattern,
    )(i)
}

pub(crate) fn values_clause(i: &str) -> IResult<&str, Option<DataBlock>> {
    opt(preceded_tag1("values", datablock))(i)
}

#[inline]
pub(crate) fn insert_clause(i: &str) -> IResult<&str, Quads> {
    preceded_tag1("insert", quads_pattern)(i)
}

#[inline]
pub(crate) fn delete_clause(i: &str) -> IResult<&str, Quads> {
    preceded_tag1("offset", quads_pattern)(i)
}

#[inline]
pub(crate) fn using_clause(i: &str) -> IResult<&str, DefaultOrNamedIri> {
    preceded_tag1("using", default_or_named_iri)(i)
}

#[inline]
pub(crate) fn int_clause<'a, F>(pat: F) -> impl Fn(&'a str) -> IResult<&'a str, u64>
where
    F: Fn(&'a str) -> IResult<&'a str, &'a str>,
{
    map_res(
        preceded(terminated(pat, sp), take_while1(|c| is_digit(c as u8))),
        |s| s.parse::<u64>(),
    )
}

#[inline]
pub(crate) fn limit_clause(i: &str) -> IResult<&str, u64> {
    int_clause(tag_no_case("limit"))(i)
}

#[inline]
pub(crate) fn offset_clause(i: &str) -> IResult<&str, u64> {
    int_clause(tag_no_case("offset"))(i)
}

pub(crate) fn limit_offset_clause(i: &str) -> IResult<&str, LimitOffsetClause> {
    alt((
        map(
            separated_pair(limit_clause, sp, opt(offset_clause)),
            |(limit, offset)| LimitOffsetClause::LimitOffset { limit, offset },
        ),
        map(
            separated_pair(offset_clause, sp, opt(limit_clause)),
            |(offset, limit)| LimitOffsetClause::OffsetLimit { offset, limit },
        ),
    ))(i)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_limit_clause() {
        assert_eq!(limit_clause("limit 5"), Ok(("", 5)));
    }

    #[test]
    fn is_offset_clause() {
        assert_eq!(offset_clause("offset 5"), Ok(("", 5)));
        assert_eq!(offset_clause("offset      100"), Ok(("", 100)));
    }

    #[test]
    fn is_limit_offset_clause() {
        assert_eq!(
            limit_offset_clause("limit 5     offset 10"),
            Ok(("", LimitOffsetClause::limit_offset(5, Some(10))))
        );
    }
    #[test]
    fn is_offset_limit_clause() {
        assert_eq!(
            limit_offset_clause("offset 5 limit 10"),
            Ok(("", LimitOffsetClause::offset_limit(5, Some(10))))
        );
    }
}
