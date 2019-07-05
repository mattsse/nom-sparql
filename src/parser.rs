use crate::expression::{ArgList, Expression, IriRef, IriRefOrFunction, PrefixedName};
use crate::node::{
    GraphNode, ObjectList, PropertyList, RdfLiteral, RdfLiteralDescriptor, TriplesNode, VarOrTerm,
};
use crate::query::{SparqlQuery, Var, VarOrIriRef};
use nom::branch::alt;
use nom::bytes::complete::{escaped, tag, tag_no_case, take_while1, take_while_m_n};
use nom::character::complete::{anychar, char, digit1, none_of, one_of};

use nom::combinator::{complete, cond, cut, map, not, opt, peek};

use crate::triple::Verb;
use nom::multi::{fold_many0, many0, separated_list};
use nom::sequence::{delimited, pair, preceded, terminated};
use nom::{
    bytes::complete::take_while,
    character::{
        complete::{alpha1, alphanumeric1},
        is_alphabetic,
    },
    dbg_dmp,
    error::{ErrorKind, ParseError},
    AsChar, Err, IResult,
};

// https://www.w3.org/TR/2004/REC-xml11-20040204/#sec-notation

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

fn string_content(i: &str) -> IResult<&str, &str> {
    escaped(none_of("'\"\\"), '\\', one_of(r#""tbnrf\'"#))(i)
}

/// https://www.w3.org/TR/rdf-sparql-query/#QSynLiterals
fn string_literal(i: &str) -> IResult<&str, &str> {
    alt((
        delimited(
            tag("'"),
            escaped(none_of("'\\"), '\\', one_of(r#""tbnrf\'"#)),
            tag("'"),
        ),
        delimited(
            tag("\""),
            escaped(none_of("\"\\"), '\\', one_of(r#""tbnrf\'"#)),
            tag("\""),
        ),
    ))(i)
}

fn rdf_literal(i: &str) -> IResult<&str, RdfLiteral> {
    map(
        pair(
            map(string_literal, String::from),
            opt(alt((
                map(language_tag, RdfLiteralDescriptor::LangTag),
                map(preceded(tag("^^"), iri_ref), RdfLiteralDescriptor::IriRef),
            ))),
        ),
        |(literal, descriptor)| RdfLiteral {
            literal,
            descriptor,
        },
    )(i)
}

fn language_tag(i: &str) -> IResult<&str, String> {
    map(
        preceded(
            char('@'),
            pair(
                alpha1,
                fold_many0(
                    pair(tag("-"), alphanumeric1),
                    String::new(),
                    |mut s, item| {
                        s += item.0;
                        s += item.1;
                        s
                    },
                ),
            ),
        ),
        |(s1, s2)| format!("{}{}", s1, s2),
    )(i)
}

#[inline]
fn echar(i: &str) -> IResult<&str, &str> {
    escaped(none_of("\\"), '\\', one_of(r#""tbnrf'"#))(i)
}

// TODO consider unicode cases in second
fn var_name(i: &str) -> IResult<&str, String> {
    map(
        pair(
            take_while_m_n(1, 1, |c| is_pn_chars_u(c) || c.is_dec_digit()),
            take_while_m_n(1, 1, |c| is_pn_chars_u(c) || c.is_dec_digit()),
        ),
        |(s1, s2)| format!("{}{}", s1, s2),
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

fn iri_ref_lex(i: &str) -> IResult<&str, &str> {
    delimited(
        tag("<"),
        take_while(|c| {
            let chrs = "<>\"{}|^\\`";
            if chrs.contains(c) {
                false
            } else {
                c as u8 > 0x20
            }
        }),
        tag(">"),
    )(i)
}

fn iri_ref(i: &str) -> IResult<&str, IriRef> {
    alt((
        map(iri_ref_lex, |i| IriRef::IriRef(i.to_string())),
        map(prefixed_name, IriRef::PrefixedName),
    ))(i)
}

fn iri_ref_or_fun(i: &str) -> IResult<&str, IriRefOrFunction> {
    unimplemented!()
}

fn var_or_iri_ref(i: &str) -> IResult<&str, VarOrIriRef> {
    alt((
        map(var, VarOrIriRef::Var),
        map(iri_ref, VarOrIriRef::IriRef),
    ))(i)
}

fn var_or_term(i: &str) -> IResult<&str, VarOrTerm> {
    unimplemented!()
}
fn graph_term(i: &str) -> IResult<&str, VarOrTerm> {
    unimplemented!()
}
fn graph_node(i: &str) -> IResult<&str, GraphNode> {
    unimplemented!()
}
fn object_list(i: &str) -> IResult<&str, ObjectList> {
    map(separated_list(tag(","), graph_node), ObjectList)(i)
}
fn triples_node(i: &str) -> IResult<&str, TriplesNode> {
    unimplemented!()
}

fn property_list(i: &str) -> IResult<&str, PropertyList> {
    unimplemented!()
}

fn verb(i: &str) -> IResult<&str, Verb> {
    alt((
        map(var_or_iri_ref, Verb::VarOrIriRef),
        map(tag_no_case("a"), |_| Verb::A),
    ))(i)
}

fn arg_list(i: &str) -> IResult<&str, ArgList> {
    unimplemented!()
}

fn expression(i: &str) -> IResult<&str, Expression> {
    unimplemented!()
}

fn var(i: &str) -> IResult<&str, Var> {
    alt((
        map(preceded(char('?'), preceded(sp, var_name)), Var::Var1),
        map(preceded(char('$'), preceded(sp, var_name)), Var::Var2),
    ))(i)
}

#[inline]
fn pn_tail(i: &str) -> IResult<&str, &str> {
    take_while(|c| is_pn_char(c) || c == '.')(i)
}

fn pn_any<'a, F>(pat: F) -> impl Fn(&'a str) -> IResult<&'a str, String>
where
    F: Fn(&'a str) -> IResult<&'a str, &'a str>,
{
    map(pair(pat, pn_tail), |(s1, tail)| format!("{}{}", s1, tail))
}

fn pn_local(i: &str) -> IResult<&str, String> {
    pn_any(take_while_m_n(1, 1, |c| is_pn_char(c) || c.is_dec_digit()))(i)
}

fn pn_prefix(i: &str) -> IResult<&str, String> {
    pn_any(pn_chars_base_one)(i)
}

fn pname_ns(i: &str) -> IResult<&str, Option<String>> {
    terminated(opt(pn_prefix), preceded(sp, char(':')))(i)
}

fn pname_ln(i: &str) -> IResult<&str, (Option<String>, String)> {
    pair(pname_ns, preceded(sp, pn_local))(i)
}

fn is_unicode(c: char) -> bool {
    match c {
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

#[inline]
fn is_illegal_char_lit_1(c: char) -> bool {
    match c {
        '\u{0027}' | '\u{005C}' | '\u{000A}' | '\u{000D}' => true,
        _ => false,
    }
}

#[inline]
fn is_illegal_char_lit_2(c: char) -> bool {
    if !is_illegal_char_lit_1(c) {
        c == '\u{0022}'
    } else {
        true
    }
}

#[inline]
fn is_pn_chars_base(i: char) -> bool {
    is_alphabetic(i as u8) || is_unicode(i)
}

#[inline]
fn pn_chars_base_one(i: &str) -> IResult<&str, &str> {
    take_while_m_n(1, 1, is_pn_chars_base)(i)
}

#[inline]
fn pn_chars_base1(i: &str) -> IResult<&str, &str> {
    take_while1(is_pn_chars_base)(i)
}

#[inline]
fn is_pn_chars_u(i: char) -> bool {
    is_pn_chars_base(i) || i == '_'
}

#[inline]
fn pn_chars_u_one(i: &str) -> IResult<&str, &str> {
    alt((pn_chars_base_one, tag("_")))(i)
}

#[inline]
fn pn_chars_u1(i: &str) -> IResult<&str, &str> {
    take_while1(is_pn_chars_u)(i)
}

#[inline]
fn is_pn_char(i: char) -> bool {
    is_pn_chars_u(i) || i == '-' || i.is_dec_digit()
}

#[inline]
fn pn_chars_one(i: &str) -> IResult<&str, &str> {
    take_while_m_n(1, 1, is_pn_char)(i)
}

#[inline]
fn pn_chars1(i: &str) -> IResult<&str, &str> {
    take_while1(is_pn_char)(i)
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
        assert_eq!(pn_chars_base_one("a "), Ok((" ", "a")));
        assert_eq!(pn_chars_base_one("\u{00C0} "), Ok((" ", "\u{00C0}")));
        assert_eq!(pn_chars_base_one("\u{03b1} "), Ok((" ", "\u{03b1}")));
        assert_eq!(pn_chars_base_one("\u{00D8} "), Ok((" ", "\u{00D8}")));
        assert_eq!(pn_chars_base_one("\u{FDF0} "), Ok((" ", "\u{FDF0}")));
        assert_eq!(pn_chars_base_one("\u{F900} "), Ok((" ", "\u{F900}")));
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

    #[test]
    fn is_string_literal() {
        assert_eq!(
            string_literal(r#""some string lit""#),
            Ok(("", r#"some string lit"#))
        );
        assert_eq!(
            string_literal("'some string lit'"),
            Ok(("", "some string lit"))
        );
        assert_eq!(
            string_literal("'some \tstring\n\r\"  lit'"),
            Ok(("", "some \tstring\n\r\"  lit"))
        );
    }

    #[test]
    fn is_lang_tag() {
        assert_eq!(language_tag("@en"), Ok(("", "en".to_string())));
        assert_eq!(
            language_tag("@some-lang-tag1"),
            Ok(("", "some-lang-tag1".to_string()))
        );
        assert_eq!(
            language_tag("@some-123lang-tag1"),
            Ok(("", "some-123lang-tag1".to_string()))
        );
        assert_eq!(
            language_tag("@1lang"),
            Err(Err::Error(("1lang", ErrorKind::Alpha)))
        );
    }

    #[test]
    fn is_rdf_literal() {
        assert_eq!(rdf_literal("'chat'"), Ok(("", RdfLiteral::literal("chat"))));

        assert_eq!(
            rdf_literal("'chat'@fr"),
            Ok((
                "",
                RdfLiteral::new("chat", RdfLiteralDescriptor::LangTag("fr".to_string()))
            ))
        );
        assert_eq!(
            rdf_literal("'xyz'^^<http://example.org/ns/userDatatype>"),
            Ok((
                "",
                RdfLiteral::new(
                    "xyz",
                    RdfLiteralDescriptor::IriRef(IriRef::IriRef(
                        "http://example.org/ns/userDatatype".to_string()
                    ))
                )
            ))
        );
        assert_eq!(
            rdf_literal(r#""abc"^^appNS:appDataType"#),
            Ok((
                "",
                RdfLiteral::new(
                    "abc",
                    RdfLiteralDescriptor::IriRef(IriRef::PrefixedName(PrefixedName::PnameLN {
                        pn_prefix: Some("appNS".to_string()),
                        pn_local: "appDataType".to_string()
                    }))
                )
            ))
        );
    }
}
