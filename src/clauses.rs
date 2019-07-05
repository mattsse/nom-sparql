use crate::parser::sp;

use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, tag_no_case, take_while1, take_while_m_n},
    character::is_digit,
    combinator::map_res,
    sequence::{preceded, terminated},
    IResult,
};

use crate::query::LimitOffsetClause;
use nom::combinator::{map, opt};
use nom::sequence::separated_pair;

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
