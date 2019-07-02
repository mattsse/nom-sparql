use crate::expression::{IriRef, PrefixedName};
use crate::query::{SparqlQuery, Var};
use nom::branch::alt;
use nom::bytes::complete::{escaped, is_not, tag, take_while1};
use nom::character::complete::{char, digit1, one_of};
use nom::combinator::{cut, map, opt};
use nom::multi::{many0, many_till};
use nom::sequence::{pair, preceded, terminated};
use nom::{
    bytes::complete::take_while,
    character::{
        complete::{alpha1 as alpha, alphanumeric1 as alphanumeric},
        is_alphabetic,
    },
    error::{ErrorKind, ParseError},
    AsChar, Err, IResult,
};

fn sparql_query(_i: &[u8]) -> IResult<&[u8], SparqlQuery> {
    unimplemented!()
}

pub fn parse_query_bytes<T>(input: T) -> Result<SparqlQuery, &'static str>
where
    T: AsRef<[u8]>,
{
    match sparql_query(input.as_ref()) {
        Ok((_, o)) => Ok(o),
        Err(_) => Err("failed to parse query"),
    }
}

pub fn parse_query<T>(input: T) -> Result<SparqlQuery, &'static str>
where
    T: AsRef<str>,
{
    parse_query_bytes(input.as_ref().trim().as_bytes())
}

/// first we write parsers for the smallest elements (here a space character),
/// then we'll combine them in larger parsers
fn sp<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    let chars = " \t\n\r";
    take_while(move |c| chars.contains(c))(i)
}

fn parse_str<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    // https://github.com/thejpster/monotronian/blob/master/src/lexer/mod.rs#L696
    escaped(alphanumeric, '\\', one_of("\"n\\"))(i)
}

fn is_echar(i: char) -> bool {
    let echars = "\\\tb\n\rf\"'";
    echars.contains(i)
}

fn var_name(i: &str) -> IResult<&str, String> {
    map(
        pair(alt((pn_chars_u, digit1)), many0(alt((pn_chars_u, digit1)))),
        |(s1, s2)| format!("{}{}", s1, s2.concat()),
    )(i)
}

fn prefixed_name(i: &str) -> IResult<&str, PrefixedName> {
    alt((
        map(pname_ln, |(pn_prefix, pn_local)| PrefixedName::PnameLN {
            pn_prefix,
            pn_local,
        }),
        map(pname_ns, PrefixedName::PnameNS),
    ))(i)
}

fn iri_ref_lex(i: &str) -> IResult<&str, String> {
    map(
        preceded(
            char('<'),
            cut(many_till(alt((is_not("<>\"{}|^\\`"), pn_chars)), char('>'))),
        ),
        |(c, _)| c.concat(),
    )(i)
}

fn iri_ref(i: &str) -> IResult<&str, IriRef> {
    alt((
        map(iri_ref_lex, IriRef::IriRef),
        map(prefixed_name, IriRef::PrefixedName),
    ))(i)
}

fn var(i: &str) -> IResult<&str, Var> {
    alt((
        map(preceded(char('?'), preceded(sp, var_name)), Var::Var1),
        map(preceded(char('$'), preceded(sp, var_name)), Var::Var2),
    ))(i)
}

fn pn_tail(i: &str) -> IResult<&str, Option<(Vec<&str>, &str)>> {
    opt(pair(many0(alt((pn_chars, tag(".")))), pn_chars))(i)
}

fn pn_any<'a, F>(pat: F) -> impl Fn(&'a str) -> IResult<&'a str, String>
where
    F: Fn(&'a str) -> IResult<&'a str, &'a str>,
{
    map(pair(pat, pn_tail), |(s1, s2)| {
        if let Some((chain, last)) = s2 {
            format!("{}{}{}", s1, chain.concat(), last)
        } else {
            s1.to_string()
        }
    })
}

fn pn_local(i: &str) -> IResult<&str, String> {
    pn_any(alt((pn_chars_u, digit1)))(i)
}

fn pn_prefix(i: &str) -> IResult<&str, String> {
    pn_any(pn_chars_base)(i)
}

fn pname_ns(i: &str) -> IResult<&str, Option<String>> {
    terminated(opt(pn_prefix), preceded(sp, char(':')))(i)
}

fn pname_ln(i: &str) -> IResult<&str, (Option<String>, String)> {
    pair(pname_ns, preceded(sp, pn_local))(i)
}

fn is_unicode(data: char) -> bool {
    match data {
        '\u{00C0}'..='\u{00D6}' => true,
        '\u{00D8}'..='\u{00F6}' => true,
        '\u{00F8}'..='\u{02FF}' => true,
        '\u{0370}'..='\u{037D}' => true,
        '\u{037F}'..='\u{1FFF}' => true,
        '\u{200C}'..='\u{200D}' => true,
        '\u{2070}'..='\u{218F}' => true,
        '\u{2C00}'..='\u{2FEF}' => true,
        '\u{3001}'..='\u{D7FF}' => true,
        '\u{F900}'..='\u{FDCF}' => true,
        '\u{FDF0}'..='\u{FFFD}' => true,
        _ => false,
    }
}

fn pn_chars_base(i: &str) -> IResult<&str, &str> {
    take_while1(|c| is_alphabetic(c as u8) || is_unicode(c))(i)
}
fn pn_chars_u(i: &str) -> IResult<&str, &str> {
    alt((pn_chars_base, tag("_")))(i)
}

fn pn_chars(i: &str) -> IResult<&str, &str> {
    alt((pn_chars_u, tag("-"), digit1))(i)
}

fn anon(i: &str) -> IResult<&str, &str> {
    preceded(char('['), cut(terminated(sp, char(']'))))(i)
}

fn nil(i: &str) -> IResult<&str, &str> {
    preceded(char('('), cut(terminated(sp, char(')'))))(i)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_anon() {
        assert_eq!(anon("[]"), Ok(("", "")));
        assert_eq!(anon("[   ]"), Ok(("", "   ")));
        assert_eq!(anon("[a]"), Err(Err::Failure(("a]", ErrorKind::Char))));
    }

    #[test]
    fn is_pn_chars_base() {
        assert_eq!(pn_chars_base("a "), Ok((" ", "a")));
        assert_eq!(pn_chars_base("\u{00C0} "), Ok((" ", "\u{00C0}")));
        assert_eq!(pn_chars_base("\u{03b1} "), Ok((" ", "\u{03b1}")));
        assert_eq!(pn_chars_base("\u{00D8} "), Ok((" ", "\u{00D8}")));
        assert_eq!(pn_chars_base("\u{FDF0} "), Ok((" ", "\u{FDF0}")));
        assert_eq!(pn_chars_base("\u{F900} "), Ok((" ", "\u{F900}")));
    }

    #[test]
    fn is_iri_ref() {
        assert_eq!(
            iri_ref("<http://education.data.gov.uk/def/school/>"),
            Ok((
                "",
                IriRef::IriRef("http://education.data.gov.uk/def/school/".to_string())
            ))
        );
    }
}
