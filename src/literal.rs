use nom::combinator::{cut, opt, recognize};
use nom::error::{make_error, ErrorKind};
use nom::sequence::{pair, tuple};
use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, tag_no_case, take_while1, take_while_m_n},
    character::{
        complete::{char, digit1},
        is_digit,
    },
    combinator::{map, map_res},
    error::ParseError,
    sequence::{preceded, terminated},
    Err, IResult, ParseTo,
};

use crate::arithmetic::Sign;

#[derive(Debug, Clone)]
pub enum NumericLiteral {
    Int(i64),
    /// a sparql decimal is just sparql double without an exponent
    Double((f64, Option<i64>)),
}

#[inline]
pub(crate) fn silent(i: &str) -> IResult<&str, bool> {
    map(tag_no_case("silent"), |_| true)(i)
}

#[inline]
pub(crate) fn distinct(i: &str) -> IResult<&str, bool> {
    map(tag_no_case("distinct"), |_| true)(i)
}

pub(crate) fn boolean(i: &str) -> IResult<&str, bool> {
    alt((
        map(tag_no_case("false"), |_| false),
        map(tag_no_case("true"), |_| true),
    ))(i)
}

pub(crate) fn sign(i: &str) -> IResult<&str, Sign> {
    alt((map(char('+'), |_| Sign::POS), map(char('-'), |_| Sign::NEG)))(i)
}

pub(crate) fn exponent(i: &str) -> IResult<&str, i64> {
    preceded(
        alt((char('e'), char('E'))),
        map_res(
            recognize(tuple((opt(alt((char('+'), char('-')))), cut(digit1)))),
            |s: &str| s.parse::<i64>(),
        ),
    )(i)
}

pub(crate) fn recognize_spaqrql_float(i: &str) -> IResult<&str, &str> {
    recognize(pair(
        opt(alt((char('+'), char('-')))),
        alt((
            map(pair(digit1, opt(pair(char('.'), opt(digit1)))), |_| ()),
            map(tuple((char('.'), digit1)), |_| ()),
        )),
    ))(i)
}

pub(crate) fn sparql_decimal(input: &str) -> IResult<&str, f64> {
    match recognize_spaqrql_float(input) {
        Err(e) => Err(e),
        Ok((i, s)) => match s.parse_to() {
            Some(n) => Ok((i, n)),
            None => Err(Err::Error(make_error(i, ErrorKind::Float))),
        },
    }
}

pub(crate) fn sparql_double(i: &str) -> IResult<&str, (f64, Option<i64>)> {
    pair(sparql_decimal, opt(exponent))(i)
}

pub(crate) fn sparql_int(i: &str) -> IResult<&str, i64> {
    match recognize(pair(opt(alt((char('+'), char('-')))), digit1))(i) {
        Ok((i, o)) => {
            // validate that no decimal/double is present
            if i.chars()
                .next()
                .map_or_else(|| false, |c| ".eE".contains(c))
            {
                Err(Err::Error(make_error(i, ErrorKind::Float)))
            } else if let Ok(v) = o.parse() {
                Ok((i, v))
            } else {
                Err(Err::Error(make_error(i, ErrorKind::Digit)))
            }
        }
        Err(e) => Err(e),
    }
}

pub(crate) fn numeric_literal(i: &str) -> IResult<&str, NumericLiteral> {
    alt((
        map(sparql_int, NumericLiteral::Int),
        map(sparql_double, NumericLiteral::Double),
    ))(i)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_exponent() {
        assert_eq!(exponent("e+25"), Ok(("", 25)));
        assert_eq!(exponent("e25"), Ok(("", 25)));
        assert_eq!(exponent("E-25"), Ok(("", -25)));
    }

    #[test]
    fn is_decimal() {
        assert_eq!(sparql_decimal("-.0"), Ok(("", 0.0)));
        assert_eq!(sparql_decimal(".02"), Ok(("", 0.02)));
        assert_eq!(sparql_decimal("1."), Ok(("", 1.0)));
    }
    #[test]
    fn is_double() {
        assert_eq!(sparql_double("-.0"), Ok(("", (0.0, None))));
        assert_eq!(sparql_double(".02e10"), Ok(("", (0.02, Some(10)))));
        assert_eq!(sparql_double("1.E-20"), Ok(("", (1.0, Some(-20)))));
    }

    #[test]
    fn is_numeric_literal_int() {
        let mut expected = numeric_literal("+11").unwrap();

        if let NumericLiteral::Int(i) = expected.1 {
            assert_eq!(11, i);
        } else {
            panic!("int literal mistaken for double {:?}", expected.1)
        }

        expected = numeric_literal("-5").unwrap();
        if let NumericLiteral::Int(i) = expected.1 {
            assert_eq!(-5, i);
        } else {
            panic!("int literal mistaken for double {:?}", expected.1)
        }
    }
    #[test]
    fn is_numeric_literal_double() {
        let mut expected = numeric_literal("+11.").unwrap();
        if let NumericLiteral::Double((_d, e)) = expected.1 {
            assert!(e.is_none());
        } else {
            panic!("double literal mistaken for int {:?}", expected.1)
        }

        expected = numeric_literal("+.0e10").unwrap();
        if let NumericLiteral::Double((_d, e)) = expected.1 {
            assert_eq!(e, Some(10));
        } else {
            panic!("double literal mistaken for int {:?}", expected.1)
        }
    }

}
