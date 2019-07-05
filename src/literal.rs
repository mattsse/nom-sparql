


use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, tag_no_case, take_while1, take_while_m_n},
    character::is_digit,
    combinator::map_res,
    sequence::{preceded, terminated},
    IResult,
};

use crate::expression::{Sign};


use nom::character::complete::{char, digit1};
use nom::combinator::{map};
use nom::error::ParseError;




#[derive(Debug, Clone)]
pub enum NumericLiteral {
    Int(i64),
    Decimal((Option<Sign>, Vec<u8>)),
    Double(f64),
}

pub(crate) fn boolean<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, bool, E> {
    alt((
        map(tag_no_case("false"), |_| false),
        map(tag_no_case("true"), |_| true),
    ))(i)
}

pub(crate) fn sign(i: &str) -> IResult<&str, Sign> {
    alt((map(char('+'), |_| Sign::POS), map(char('-'), |_| Sign::NEG)))(i)
}

pub(crate) fn decimal(i: &str) -> IResult<&str, &[u8]> {
    map(preceded(tag("."), digit1), str::as_bytes)(i)
}

pub(crate) fn numeric_literal(_i: &str) -> IResult<&str, NumericLiteral> {
    unimplemented!()
    //    map_res(
    //        tuple((opt(sign), digit0, opt(decimal))),
    //        |(sign, s1, s2)| {
    //            if s1.is_empty(){
    //                if let { }
    //
    //                return NumericLiteral::Decimal((sign, s2.collect()))
    //            }
    //        },
    //    )(i)
}
