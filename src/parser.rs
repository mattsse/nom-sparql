use nom::{
    branch::alt,
    bytes::complete::take_while,
    bytes::complete::{escaped, tag, tag_no_case, take_while1, take_while_m_n},
    character::complete::{anychar, char, digit1, none_of, one_of},
    character::{
        complete::{alpha1, alphanumeric1},
        is_alphabetic,
    },
    combinator::{complete, cond, cut, map, map_res, not, opt, peek},
    error::ErrorKind,
    multi::{fold_many0, separated_list},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    AsChar, Err, IResult,
};

use crate::aggregate::count;
use crate::{
    ask::ask_query,
    call::arg_list,
    clauses::values_clause,
    construct::construct_query,
    data::datablock,
    describe::describe_query,
    expression::{DefaultOrNamedIri, Iri, IriOrFunction, PrefixedName},
    graph::graph_term,
    node::{
        Collection, ObjectList, PropertyList, RdfLiteral, RdfLiteralDescriptor, TriplesNode,
        VarOrTerm, VerbList,
    },
    query::{
        BaseOrPrefixDecl, PrefixDecl, Prologue, SparqlQuery, SparqlQueryStatement, Var, VarOrIri,
    },
    select::select_query,
};
use nom::character::{is_alphanumeric, is_digit};
use nom::combinator::recognize;
use nom::multi::{many0, many1};

pub fn sparql_query_stmt(i: &str) -> IResult<&str, SparqlQueryStatement> {
    map(
        tuple((
            terminated(prologue, sp),
            sparql_query,
            opt(preceded(sp1, preceded_tag1("values", datablock))),
        )),
        |(prologue, query, values)| SparqlQueryStatement {
            prologue,
            query,
            values,
        },
    )(i)
}

pub fn sparql_query(i: &str) -> IResult<&str, SparqlQuery> {
    alt((
        map(select_query, SparqlQuery::Select),
        map(construct_query, SparqlQuery::Construct),
        map(describe_query, SparqlQuery::Describe),
        map(ask_query, SparqlQuery::Ask),
    ))(i)
}

pub fn parse_query_bytes<T>(input: T) -> Result<SparqlQuery, &'static str>
where
    T: AsRef<[u8]>,
{
    unimplemented!()

    //    match sparql_query(input.as_ref()) {
    //        Ok((_, o)) => Ok(o),
    //        Err(_) => Err("failed to parse query"),
    //    }
}

pub fn parse_query<T>(input: T) -> Result<SparqlQuery, &'static str>
where
    T: AsRef<str>,
{
    parse_query_bytes(input.as_ref().trim().as_bytes())
}

#[inline]
pub(crate) fn preceded_bracketted<'a, O1, F>(
    tag: &'a str,
    pat: F,
) -> impl Fn(&'a str) -> IResult<&'a str, O1>
where
    F: Fn(&'a str) -> IResult<&'a str, O1>,
{
    preceded_tag(tag, bracketted(pat))
}

#[inline]
pub(crate) fn bracketted<'a, O1, F>(pat: F) -> impl Fn(&'a str) -> IResult<&'a str, O1>
where
    F: Fn(&'a str) -> IResult<&'a str, O1>,
{
    delimited(char('('), pat, char(')'))
}

#[inline]
pub(crate) fn preceded_tag1<'a, O1, F>(
    tag: &'a str,
    pat: F,
) -> impl Fn(&'a str) -> IResult<&'a str, O1>
where
    F: Fn(&'a str) -> IResult<&'a str, O1>,
{
    preceded(terminated(tag_no_case(tag), sp1), pat)
}

#[inline]
pub(crate) fn preceded_tag<'a, O1, F>(
    tag: &'a str,
    pat: F,
) -> impl Fn(&'a str) -> IResult<&'a str, O1>
where
    F: Fn(&'a str) -> IResult<&'a str, O1>,
{
    preceded(terminated(tag_no_case(tag), sp), pat)
}

#[inline]
pub(crate) fn sp(i: &str) -> IResult<&str, &str> {
    take_while(is_sp)(i)
}

#[inline]
pub(crate) fn sp1(i: &str) -> IResult<&str, &str> {
    take_while1(is_sp)(i)
}

#[inline]
pub(crate) fn sp_enc<'a, O1, F>(pat: F) -> impl Fn(&'a str) -> IResult<&'a str, O1>
where
    F: Fn(&'a str) -> IResult<&'a str, O1>,
{
    delimited(sp, pat, sp)
}

#[inline]
pub(crate) fn sp_enc1<'a, O1, F>(pat: F) -> impl Fn(&'a str) -> IResult<&'a str, O1>
where
    F: Fn(&'a str) -> IResult<&'a str, O1>,
{
    delimited(sp1, pat, sp1)
}

#[inline]
pub(crate) fn sp_sep1<'a, O1, O2, F, G>(
    first: F,
    second: G,
) -> impl Fn(&'a str) -> IResult<&'a str, (O1, O2)>
where
    F: Fn(&'a str) -> IResult<&'a str, O1>,
    G: Fn(&'a str) -> IResult<&'a str, O2>,
{
    separated_pair(first, sp1, second)
}

#[inline]
pub(crate) fn sp_sep<'a, O1, O2, F, G>(
    first: F,
    second: G,
) -> impl Fn(&'a str) -> IResult<&'a str, (O1, O2)>
where
    F: Fn(&'a str) -> IResult<&'a str, O1>,
    G: Fn(&'a str) -> IResult<&'a str, O2>,
{
    separated_pair(first, sp, second)
}

pub(crate) fn string_content(i: &str) -> IResult<&str, &str> {
    escaped(none_of("'\"\\"), '\\', one_of(r#""tbnrf\'"#))(i)
}

/// https://www.w3.org/TR/rdf-sparql-query/#QSynLiterals
pub(crate) fn string_literal(i: &str) -> IResult<&str, &str> {
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

pub(crate) fn rdf_literal(i: &str) -> IResult<&str, RdfLiteral> {
    map(
        pair(
            map(string_literal, String::from),
            opt(alt((
                map(language_tag, RdfLiteralDescriptor::LangTag),
                map(preceded(tag("^^"), iri), RdfLiteralDescriptor::IriRef),
            ))),
        ),
        |(literal, descriptor)| RdfLiteral {
            literal,
            descriptor,
        },
    )(i)
}

// TODO refactor
pub(crate) fn language_tag(i: &str) -> IResult<&str, String> {
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
pub(crate) fn echar(i: &str) -> IResult<&str, &str> {
    escaped(none_of("\\"), '\\', one_of(r#""tbnrf'"#))(i)
}

// TODO consider unicode cases in second
pub(crate) fn var_name(i: &str) -> IResult<&str, &str> {
    recognize(pair(
        take_while_m_n(1, 1, |c| is_pn_chars_u(c) || c.is_dec_digit()),
        take_while(|c| is_pn_chars_u(c) || c.is_dec_digit()),
    ))(i)
}

pub(crate) fn prefixed_name(i: &str) -> IResult<&str, PrefixedName> {
    alt((
        map(pname_ln, |(pn_prefix, pn_local)| PrefixedName::PnameLN {
            pn_prefix,
            pn_local,
        }),
        map(pname_ns, PrefixedName::PnameNS),
    ))(i)
}

pub(crate) fn base_decl(i: &str) -> IResult<&str, &str> {
    preceded(terminated(tag_no_case("base"), sp1), iri_ref)(i)
}

pub(crate) fn prefix_decl(i: &str) -> IResult<&str, PrefixDecl> {
    map(
        tuple((
            tag_no_case("prefix"),
            sp_enc(pname_ns),
            map(iri_ref, str::to_string),
        )),
        |(_, pname_ns, iri_ref)| PrefixDecl { pname_ns, iri_ref },
    )(i)
}

pub(crate) fn prologue(i: &str) -> IResult<&str, Prologue> {
    map(
        separated_list(
            sp1,
            alt((
                map(base_decl, |s| BaseOrPrefixDecl::Base(s.to_string())),
                map(prefix_decl, BaseOrPrefixDecl::Prefix),
            )),
        ),
        Prologue,
    )(i)
}

pub(crate) fn iri_ref(i: &str) -> IResult<&str, &str> {
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

pub(crate) fn named_iri(i: &str) -> IResult<&str, Iri> {
    preceded_tag1(
        "named",
        alt((
            map(iri_ref, |i| Iri::Iri(i.to_string())),
            map(prefixed_name, Iri::PrefixedName),
        )),
    )(i)
}

pub(crate) fn default_or_named_iri(i: &str) -> IResult<&str, DefaultOrNamedIri> {
    alt((
        map(named_iri, DefaultOrNamedIri::Named),
        map(iri, DefaultOrNamedIri::Default),
    ))(i)
}

pub(crate) fn iri(i: &str) -> IResult<&str, Iri> {
    alt((
        map(iri_ref, |i| Iri::Iri(i.to_string())),
        map(prefixed_name, Iri::PrefixedName),
    ))(i)
}

pub(crate) fn iri_or_fun(i: &str) -> IResult<&str, IriOrFunction> {
    map(
        pair(iri, opt(preceded(sp1, arg_list))),
        |(iri, arg_list)| IriOrFunction { iri, arg_list },
    )(i)
}

pub(crate) fn var_or_iri(i: &str) -> IResult<&str, VarOrIri> {
    alt((map(var, VarOrIri::Var), map(iri, VarOrIri::Iri)))(i)
}

pub(crate) fn var_or_term(i: &str) -> IResult<&str, VarOrTerm> {
    alt((map(var, VarOrTerm::Var), map(graph_term, VarOrTerm::Term)))(i)
}

pub(crate) fn var(i: &str) -> IResult<&str, Var> {
    alt((
        map(
            preceded(char('?'), preceded(sp, map(var_name, str::to_string))),
            Var::QMark,
        ),
        map(
            preceded(char('$'), preceded(sp, map(var_name, str::to_string))),
            Var::Dollar,
        ),
    ))(i)
}

#[inline]
pub(crate) fn pn_chars_tail(i: &str) -> IResult<&str, &str> {
    // ((PN_CHARS|'.')* PN_CHARS)?
    recognize(pair(
        opt(take_while1(|c| c == '.')),
        separated_list(take_while1(|c| c == '.'), pn_chars1),
    ))(i)
}

fn from_hex(input: &str) -> Result<u8, std::num::ParseIntError> {
    u8::from_str_radix(input, 16)
}
fn is_hex_digit(c: char) -> bool {
    c.is_digit(16)
}

fn hex_primary(i: &str) -> IResult<&str, &str> {
    take_while_m_n(2, 2, is_hex_digit)(i)
}

fn plx(i: &str) -> IResult<&str, &str> {
    recognize(alt((
        pair(tag("%"), hex_primary),
        pair(tag("\\"), recognize(one_of("_~.-!$&'()*+,;=/?#@%"))),
    )))(i)
}

pub(crate) fn pn_local(i: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((
            pn_chars_u_one,
            tag(":"),
            take_while_m_n(1, 1, |c| is_digit(c as u8)),
            plx,
        )),
        recognize(pair(
            opt(take_while1(|c| c == '.')),
            separated_list(
                take_while1(|c| c == '.'),
                many0(alt((take_while1(|c| is_pn_char(c) || c == ':'), plx))),
            ),
        )),
    ))(i)
}

pub(crate) fn pn_prefix(i: &str) -> IResult<&str, &str> {
    recognize(pair(pn_chars_base_one, pn_chars_tail))(i)
}

pub(crate) fn pname_ns(i: &str) -> IResult<&str, Option<String>> {
    terminated(opt(map(pn_prefix, str::to_string)), preceded(sp, char(':')))(i)
}

pub(crate) fn pname_ln(i: &str) -> IResult<&str, (Option<String>, String)> {
    pair(pname_ns, preceded(sp, map(pn_local, str::to_string)))(i)
}

#[inline]
pub(crate) fn is_sp(c: char) -> bool {
    " \t\n\r".contains(c)
}

pub(crate) fn is_unicode(c: char) -> bool {
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
pub(crate) fn is_illegal_char_lit_1(c: char) -> bool {
    match c {
        '\u{0027}' | '\u{005C}' | '\u{000A}' | '\u{000D}' => true,
        _ => false,
    }
}

#[inline]
pub(crate) fn is_illegal_char_lit_2(c: char) -> bool {
    if !is_illegal_char_lit_1(c) {
        c == '\u{0022}'
    } else {
        true
    }
}

#[inline]
pub(crate) fn is_pn_chars_base(i: char) -> bool {
    is_alphabetic(i as u8) || is_unicode(i)
}

#[inline]
pub(crate) fn pn_chars_base_one(i: &str) -> IResult<&str, &str> {
    take_while_m_n(1, 1, is_pn_chars_base)(i)
}

#[inline]
pub(crate) fn pn_chars_base1(i: &str) -> IResult<&str, &str> {
    take_while1(is_pn_chars_base)(i)
}

#[inline]
pub(crate) fn is_pn_chars_u(i: char) -> bool {
    is_pn_chars_base(i) || i == '_'
}

#[inline]
pub(crate) fn pn_chars_u_one(i: &str) -> IResult<&str, &str> {
    alt((pn_chars_base_one, tag("_")))(i)
}

#[inline]
pub(crate) fn pn_chars_u1(i: &str) -> IResult<&str, &str> {
    take_while1(is_pn_chars_u)(i)
}

#[inline]
pub(crate) fn is_pn_char(i: char) -> bool {
    is_pn_chars_u(i) || i == '-' || i.is_dec_digit()
}

#[inline]
pub(crate) fn pn_chars_one(i: &str) -> IResult<&str, &str> {
    take_while_m_n(1, 1, is_pn_char)(i)
}

#[inline]
pub(crate) fn pn_chars1(i: &str) -> IResult<&str, &str> {
    take_while1(is_pn_char)(i)
}

pub(crate) fn anon(i: &str) -> IResult<&str, &str> {
    preceded(char('['), cut(terminated(sp, char(']'))))(i)
}

pub(crate) fn nil(i: &str) -> IResult<&str, &str> {
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
            iri("<http://education.data.gov.uk/def/school/>"),
            Ok((
                "",
                Iri::Iri("http://education.data.gov.uk/def/school/".to_string())
            ))
        );
    }

    #[test]
    fn is_iri() {
        assert_eq!(
            iri("<http://education.data.gov.uk/def/school/>"),
            Ok((
                "",
                Iri::Iri("http://education.data.gov.uk/def/school/".to_string())
            ))
        );

        //        assert_eq!(
        //            iri("<http://education.data.gov.uk/def/school/>"),
        //            Ok((
        //                "",
        //                Iri::PrefixedName("http://education.data.gov.uk/def/school/".to_string())
        //            ))
        //        );
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
                    RdfLiteralDescriptor::IriRef(Iri::Iri(
                        "http://example.org/ns/userDatatype".to_string()
                    ))
                )
            ))
        );
        assert_eq!(
            rdf_literal(r#""abc"^^appNS:1app.Data.Type"#),
            Ok((
                "",
                RdfLiteral::new(
                    "abc",
                    RdfLiteralDescriptor::IriRef(Iri::PrefixedName(PrefixedName::PnameLN {
                        pn_prefix: Some("appNS".to_string()),
                        pn_local: "1app.Data.Type".to_string()
                    }))
                )
            ))
        );
        assert_eq!(
            rdf_literal(r#""abc"^^appNS:1...app.Data.Type"#),
            Ok((
                "",
                RdfLiteral::new(
                    "abc",
                    RdfLiteralDescriptor::IriRef(Iri::PrefixedName(PrefixedName::PnameLN {
                        pn_prefix: Some("appNS".to_string()),
                        pn_local: "1...app.Data.Type".to_string()
                    }))
                )
            ))
        );
    }
}
