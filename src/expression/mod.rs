use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case},
    character::complete::char,
    combinator::{map, opt},
    multi::separated_nonempty_list,
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};

use crate::{
    arithmetic::{conditional_and_expression, ConditionalAndExpression},
    call::{built_in_call, function_call, BuiltInCall, FunctionCall},
    literal::{boolean, distinct, numeric_literal, NumericLiteral},
    node::RdfLiteral,
    terminals::{bracketted, iri_or_fun, nil, preceded_tag1, rdf_literal, sp1, sp_enc, sp_enc1},
};

use crate::var::{var, Var};

pub mod functions;
pub mod iri;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Expression(pub Vec<ConditionalAndExpression>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ExpressionList {
    Nil,
    List(Vec<Expression>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PrimaryExpression {
    BrackettedExpression(Box<Expression>),
    BuiltInCall(BuiltInCall),
    IriOrFunction(IriOrFunction),
    RdfLiteral(RdfLiteral),
    NumericLiteral(NumericLiteral),
    BooleanLiteral(bool),
    Var(Var),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Iri {
    Iri(String),
    PrefixedName(PrefixedName),
}

impl Iri {
    pub fn iri_ref<T: ToString>(iri_ref: T) -> Self {
        Iri::Iri(iri_ref.to_string())
    }

    pub fn prefixed_name<T: Into<PrefixedName>>(prefixed_name: T) -> Self {
        Iri::PrefixedName(prefixed_name.into())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PrefixedName {
    PnameLN {
        pn_prefix: Option<String>,
        pn_local: String,
    },
    PnameNS(Option<String>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum DefaultOrNamedIri {
    Default(Iri),
    Named(Iri),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct IriOrFunction {
    pub iri: Iri,
    pub arg_list: Option<ArgList>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ArgList {
    Nil,
    Expression {
        distinct: bool,
        expressions: Vec<Expression>,
    },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DistinctExpression {
    pub distinct: bool,
    pub expression: Expression,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ExpressionAsVar {
    pub expression: Box<Expression>,
    pub var: Var,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ExpressionAsVarOpt {
    pub expression: Box<Expression>,
    pub var: Option<Var>,
}

pub(crate) fn bracketted3<'a, O1, F>(i: &'a str, pat: F) -> IResult<&str, (O1, O1, Option<O1>)>
where
    F: Fn(&'a str) -> IResult<&'a str, O1>,
{
    bracketted(tuple((
        &pat,
        preceded(sp_enc(char(',')), &pat),
        opt(preceded(sp_enc(char(',')), &pat)),
    )))(i)
}

pub(crate) fn bracketted_expr3(
    i: &str,
) -> IResult<&str, (Expression, Expression, Option<Expression>)> {
    bracketted3(i, expression)
}

pub(crate) fn expression_as_var_opt(i: &str) -> IResult<&str, ExpressionAsVarOpt> {
    delimited(
        char('('),
        sp_enc(map(
            pair(expression, opt(preceded(sp_enc1(tag_no_case("as")), var))),
            |(expression, var)| ExpressionAsVarOpt {
                expression: Box::new(expression),
                var,
            },
        )),
        char(')'),
    )(i)
}

pub(crate) fn expression_as_var(i: &str) -> IResult<&str, ExpressionAsVar> {
    delimited(
        char('('),
        sp_enc(map(
            separated_pair(expression, sp_enc(tag_no_case("as")), var),
            |(expression, var)| ExpressionAsVar {
                expression: Box::new(expression),
                var,
            },
        )),
        char(')'),
    )(i)
}

pub(crate) fn primary_expression(i: &str) -> IResult<&str, PrimaryExpression> {
    alt((
        map(bracketted_expression, |expr| {
            PrimaryExpression::BrackettedExpression(Box::new(expr))
        }),
        map(built_in_call, PrimaryExpression::BuiltInCall),
        map(iri_or_fun, PrimaryExpression::IriOrFunction),
        map(rdf_literal, PrimaryExpression::RdfLiteral),
        map(numeric_literal, PrimaryExpression::NumericLiteral),
        map(boolean, PrimaryExpression::BooleanLiteral),
        map(var, PrimaryExpression::Var),
    ))(i)
}

pub(crate) fn expression_list(i: &str) -> IResult<&str, ExpressionList> {
    alt((
        map(nil, |_| ExpressionList::Nil),
        map(
            delimited(
                char('('),
                separated_nonempty_list(sp_enc(char(',')), expression),
                char(')'),
            ),
            ExpressionList::List,
        ),
    ))(i)
}

pub(crate) fn distinct_expression(i: &str) -> IResult<&str, DistinctExpression> {
    map(
        pair(
            map(opt(terminated(distinct, sp1)), Option::unwrap_or_default),
            expression,
        ),
        |(distinct, expression)| DistinctExpression {
            distinct,
            expression,
        },
    )(i)
}

pub(crate) fn bracketted_expression(i: &str) -> IResult<&str, Expression> {
    bracketted(sp_enc(expression))(i)
}

pub(crate) fn bind(i: &str) -> IResult<&str, ExpressionAsVar> {
    preceded_tag1("bind", expression_as_var)(i)
}

pub(crate) fn expression(i: &str) -> IResult<&str, Expression> {
    map(
        separated_nonempty_list(sp_enc(tag("||")), conditional_and_expression),
        Expression,
    )(i)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::terminals::iri;

    #[test]
    fn is_iri() {
        assert_eq!(
            iri("<http://example.org/book/book1>"),
            Ok(("", Iri::Iri("http://example.org/book/book1".to_string())))
        );

        assert_eq!(
            iri(":book1"),
            Ok((
                "",
                Iri::PrefixedName(PrefixedName::PnameLN {
                    pn_prefix: None,
                    pn_local: "book1".to_string()
                })
            ))
        );

        assert_eq!(
            iri(":"),
            Ok(("", Iri::PrefixedName(PrefixedName::PnameNS(None))))
        );
    }

    #[test]
    fn is_expression_as_var() {}
}
