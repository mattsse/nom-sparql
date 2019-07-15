use nom::{
    branch::alt,
    bytes::complete::tag_no_case,
    character::complete::char,
    combinator::{map, opt},
    multi::separated_nonempty_list,
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult,
};

use crate::{
    aggregate::{aggregate, Aggregate},
    expression::{
        bracketted_expression, expression, expression_list, regex_expression,
        str_replace_expression, substring_expression, ArgList, Expression, ExpressionList, Iri,
        RegexExpression, StrReplaceExpression, SubstringExpression,
    },
    graph::{group_graph_pattern, GroupGraphPattern},
    literal::distinct,
    terminals::{iri, nil, preceded_bracketted, preceded_tag1, sp, sp1, sp_enc, sp_enc1},
    var::{var, Var},
};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionCall {
    pub iri: Iri,
    pub args: ArgList,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BuiltInCall {
    Aggregate(Aggregate),
    Str(Expression),
    Lang(Expression),
    LangMatches((Expression, Expression)),
    Datatype(Expression),
    Bound(Var),
    Iri(Expression),
    Uri(Expression),
    Bnode(Option<Expression>),
    Rand,
    Abs(Expression),
    Ceil(Expression),
    Floor(Expression),
    Round(Expression),
    Concat((ExpressionList, SubstringExpression)),
    StrLen((Expression, StrReplaceExpression)),
    UCase(Expression),
    LCase(Expression),
    EncodeForUri(Expression),
    Contains((Expression, Expression)),
    StrStarts((Expression, Expression)),
    StrEnds((Expression, Expression)),
    StrBefore((Expression, Expression)),
    StrAfter((Expression, Expression)),
    Year(Expression),
    Month(Expression),
    Day(Expression),
    Hours(Expression),
    Minutes(Expression),
    Seconds(Expression),
    Timezone(Expression),
    Tz(Expression),
    Now,
    UUID,
    StrUUID,
    MD5(Expression),
    SHA1(Expression),
    SHA256(Expression),
    SHA384(Expression),
    SHA512(Expression),
    Coalesce(ExpressionList),
    If((Expression, Expression, Expression)),
    StrLang((Expression, Expression)),
    StrDt((Expression, Expression)),
    SameTerm((Expression, Expression)),
    IsIri(Expression),
    IsUri(Expression),
    IsBlank(Expression),
    IsLiteral(Expression),
    IsNumeric(Expression),
    Regex(RegexExpression),
    ExistsFunc(GroupGraphPattern),
    NotExistsFunc(GroupGraphPattern),
}

pub(crate) fn function_call(i: &str) -> IResult<&str, FunctionCall> {
    map(separated_pair(iri, sp, arg_list), |(iri, args)| {
        FunctionCall { iri, args }
    })(i)
}

fn built_in_call_1(i: &str) -> IResult<&str, BuiltInCall> {
    alt((
        map(aggregate, BuiltInCall::Aggregate),
        map(preceded_bracketted("str", expression), BuiltInCall::Str),
        map(preceded_bracketted("lang", expression), BuiltInCall::Lang),
        map(
            preceded_bracketted(
                "langmatches",
                separated_pair(expression, sp_enc(char(',')), expression),
            ),
            BuiltInCall::LangMatches,
        ),
        map(
            preceded_bracketted("datatype", expression),
            BuiltInCall::Datatype,
        ),
        map(preceded_bracketted("bound", var), BuiltInCall::Bound),
        map(preceded_bracketted("iri", expression), BuiltInCall::Iri),
        map(preceded_bracketted("uri", expression), BuiltInCall::Uri),
        map(
            preceded_bracketted(
                "bnode",
                alt((map(expression, Option::Some), map(nil, |_| None))),
            ),
            BuiltInCall::Bnode,
        ),
        map(preceded_bracketted("rand", nil), |_| BuiltInCall::Rand),
        map(preceded_bracketted("abs", expression), BuiltInCall::Uri),
        map(preceded_bracketted("ceil", expression), BuiltInCall::Uri),
        map(preceded_bracketted("floor", expression), BuiltInCall::Uri),
        map(preceded_bracketted("round", expression), BuiltInCall::Uri),
        map(
            preceded_tag1(
                "concat",
                separated_pair(expression_list, sp, substring_expression),
            ),
            BuiltInCall::Concat,
        ),
        map(
            preceded_tag1(
                "strlen",
                separated_pair(bracketted_expression, sp, str_replace_expression),
            ),
            BuiltInCall::StrLen,
        ),
        map(preceded_bracketted("ucase", expression), BuiltInCall::UCase),
        map(preceded_bracketted("lcase", expression), BuiltInCall::LCase),
        map(
            preceded_bracketted("encode_for_uri", expression),
            BuiltInCall::EncodeForUri,
        ),
        map(
            preceded_bracketted(
                "strlen",
                separated_pair(expression, sp_enc(char(',')), expression),
            ),
            BuiltInCall::Contains,
        ),
        map(
            preceded_bracketted(
                "strstarts",
                separated_pair(expression, sp_enc(char(',')), expression),
            ),
            BuiltInCall::StrStarts,
        ),
    ))(i)
}

fn built_in_call_2(i: &str) -> IResult<&str, BuiltInCall> {
    alt((
        map(
            preceded_bracketted(
                "strends",
                separated_pair(expression, sp_enc(char(',')), expression),
            ),
            BuiltInCall::StrEnds,
        ),
        map(
            preceded_bracketted(
                "strbefore",
                separated_pair(expression, sp_enc(char(',')), expression),
            ),
            BuiltInCall::StrBefore,
        ),
        map(
            preceded_bracketted(
                "strafter",
                separated_pair(expression, sp_enc(char(',')), expression),
            ),
            BuiltInCall::StrAfter,
        ),
        map(preceded_bracketted("year", expression), BuiltInCall::Year),
        map(preceded_bracketted("month", expression), BuiltInCall::Month),
        map(preceded_bracketted("day", expression), BuiltInCall::Day),
        map(preceded_bracketted("hours", expression), BuiltInCall::Hours),
        map(
            preceded_bracketted("minutes", expression),
            BuiltInCall::Minutes,
        ),
        map(
            preceded_bracketted("seconds", expression),
            BuiltInCall::Seconds,
        ),
        map(
            preceded_bracketted("timezone", expression),
            BuiltInCall::Timezone,
        ),
        map(preceded_bracketted("tz", expression), BuiltInCall::Tz),
        map(preceded_bracketted("now", nil), |_| BuiltInCall::Now),
        map(preceded_bracketted("uuid", nil), |_| BuiltInCall::UUID),
        map(preceded_bracketted("struuid", nil), |_| {
            BuiltInCall::StrUUID
        }),
    ))(i)
}

pub(crate) fn built_in_call(i: &str) -> IResult<&str, BuiltInCall> {
    alt((
        built_in_call_1,
        built_in_call_2,
        map(preceded_bracketted("md5", expression), BuiltInCall::MD5),
        map(preceded_bracketted("sha1", expression), BuiltInCall::SHA1),
        map(
            preceded_bracketted("sha256", expression),
            BuiltInCall::SHA256,
        ),
        map(
            preceded_bracketted("sha384", expression),
            BuiltInCall::SHA384,
        ),
        map(
            preceded_bracketted("sha512", expression),
            BuiltInCall::SHA512,
        ),
        map(
            preceded_tag1("coalesce", expression_list),
            BuiltInCall::Coalesce,
        ),
        map(
            preceded_bracketted(
                "if",
                tuple((
                    expression,
                    preceded(sp_enc(char(',')), expression),
                    preceded(sp_enc(char(',')), expression),
                )),
            ),
            BuiltInCall::If,
        ),
        map(
            preceded_bracketted(
                "strlang",
                separated_pair(expression, sp_enc(char(',')), expression),
            ),
            BuiltInCall::StrLang,
        ),
        map(
            preceded_bracketted(
                "strdt",
                separated_pair(expression, sp_enc(char(',')), expression),
            ),
            BuiltInCall::StrDt,
        ),
        map(
            preceded_bracketted(
                "sameterm",
                separated_pair(expression, sp_enc(char(',')), expression),
            ),
            BuiltInCall::SameTerm,
        ),
        map(preceded_bracketted("isiri", expression), BuiltInCall::IsIri),
        map(preceded_bracketted("isuri", expression), BuiltInCall::IsUri),
        map(
            preceded_bracketted("isblank", expression),
            BuiltInCall::IsBlank,
        ),
        map(
            preceded_bracketted("isliteral", expression),
            BuiltInCall::IsLiteral,
        ),
        map(
            preceded_bracketted("isnumeric", expression),
            BuiltInCall::IsNumeric,
        ),
        map(regex_expression, BuiltInCall::Regex),
        map(
            preceded_tag1("exists", group_graph_pattern),
            BuiltInCall::ExistsFunc,
        ),
        map(
            tuple((
                tag_no_case("not"),
                sp_enc1(tag_no_case("exists")),
                group_graph_pattern,
            )),
            |(_, _, graph)| BuiltInCall::NotExistsFunc(graph),
        ),
    ))(i)
}

pub(crate) fn arg_list(i: &str) -> IResult<&str, ArgList> {
    alt((
        map(nil, |_| ArgList::Nil),
        map(
            delimited(
                terminated(char('('), sp),
                tuple((
                    map(opt(terminated(distinct, sp1)), Option::unwrap_or_default),
                    separated_nonempty_list(sp_enc(char(',')), expression),
                )),
                char(')'),
            ),
            |(distinct, expressions)| ArgList::Expression {
                distinct,
                expressions,
            },
        ),
    ))(i)
}
