use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case},
    character::complete::char,
    combinator::map,
    multi::{many0, many1, separated_list, separated_nonempty_list},
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};

use crate::{
    expression::{DefaultOrNamedIri, Iri},
    literal::{boolean, numeric_literal, NumericLiteral},
    node::RdfLiteral,
    terminals::{bracketted, default_or_named_iri, iri, preceded_tag1, rdf_literal, sp, sp_enc},
    var::{var, Var},
};

/// NAMED GraphClause = NAMED Iri
pub type DataSetClause = DefaultOrNamedIri;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum DataBlock {
    InlineDataOneVar(InlineDataOneVar),
    InlineDataFull(InlineDataFull),
}

#[derive(Debug, Clone, Eq, PartialEq, new)]
pub struct InlineDataOneVar {
    pub var: Var,
    pub values: Vec<DataBlockValue>,
}

#[derive(Debug, Clone, Eq, PartialEq, new)]
pub struct InlineDataFull {
    pub vars: Vec<Var>,
    pub data_block_values: Vec<Vec<DataBlockValue>>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum DataBlockValue {
    Iri(Iri),
    RdfLiteral(RdfLiteral),
    NumericLiteral(NumericLiteral),
    BooleanLiteral(bool),
    /// If a variable has no value for a particular solution in the VALUES clause,
    /// the keyword UNDEF is used instead of an RDF term.
    UnDef,
}

pub(crate) fn data_set_clause(i: &str) -> IResult<&str, DataSetClause> {
    preceded_tag1("from", default_or_named_iri)(i)
}

pub(crate) fn datablock(i: &str) -> IResult<&str, DataBlock> {
    alt((
        map(inline_data_one_var, DataBlock::InlineDataOneVar),
        map(inline_data_full, DataBlock::InlineDataFull),
    ))(i)
}

pub(crate) fn inline_data(i: &str) -> IResult<&str, DataBlock> {
    preceded_tag1("values", datablock)(i)
}

//delimited(tag("("), map(var,|x|vec![x]), tag(")")),
pub(crate) fn inline_data_full(i: &str) -> IResult<&str, InlineDataFull> {
    map(
        tuple((
            bracketted(preceded(sp, many0(sp_enc(var)))),
            delimited(
                sp_enc(char('{')),
                many0(sp_enc(bracketted(preceded(
                    sp,
                    many0(sp_enc(datablock_value)),
                )))),
                preceded(sp, char('}')),
            ),
        )),
        |(vars, data_block_values)| InlineDataFull {
            vars,
            data_block_values,
        },
    )(i)
}

pub(crate) fn inline_data_one_var(i: &str) -> IResult<&str, InlineDataOneVar> {
    map(
        pair(
            var,
            delimited(
                sp_enc(char('{')),
                many0(sp_enc(datablock_value)),
                preceded(sp, char('}')),
            ),
        ),
        |(var, values)| InlineDataOneVar { var, values },
    )(i)
}

pub(crate) fn datablock_value(i: &str) -> IResult<&str, DataBlockValue> {
    alt((
        map(iri, DataBlockValue::Iri),
        map(rdf_literal, DataBlockValue::RdfLiteral),
        map(numeric_literal, DataBlockValue::NumericLiteral),
        map(boolean, DataBlockValue::BooleanLiteral),
        map(tag_no_case("undef"), |_| DataBlockValue::UnDef),
    ))(i)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::clauses::values_clause;
    use crate::expression::PrefixedName;
    use crate::node::{RdfLiteral, RdfLiteralDescriptor};

    #[test]
    fn is_datablock_value() {
        assert_eq!(
            datablock_value("true"),
            Ok(("", DataBlockValue::BooleanLiteral(true)))
        );

        assert_eq!(
            datablock_value("'chat'@fr"),
            Ok((
                "",
                DataBlockValue::RdfLiteral(RdfLiteral::new(
                    "chat",
                    RdfLiteralDescriptor::LangTag("fr".to_string())
                ))
            ))
        );

        assert_eq!(
            datablock_value(r#""abc""#),
            Ok(("", DataBlockValue::RdfLiteral(RdfLiteral::literal("abc"))))
        );
    }

    #[test]
    fn is_inline_data_one_var() {
        assert_eq!(
            inline_data_one_var(r#"?name { "abc" "def" }"#),
            Ok((
                "",
                InlineDataOneVar::new(
                    Var::QMark("name".to_string()),
                    vec![
                        DataBlockValue::RdfLiteral(RdfLiteral::literal("abc")),
                        DataBlockValue::RdfLiteral(RdfLiteral::literal("def"))
                    ]
                )
            ))
        );
    }

    #[test]
    fn is_inline_data_full() {
        assert_eq!(
            inline_data_full(r#"() { }"#),
            Ok(("", InlineDataFull::new(vec![], vec![])))
        );

        assert_eq!(
            inline_data_full("( ?name ?z) { }"),
            Ok((
                "",
                InlineDataFull::new(
                    vec![Var::QMark("name".to_string()), Var::QMark("z".to_string())],
                    vec![]
                )
            ))
        );

        assert_eq!(
            inline_data_full(r#"(?name) {  ("abc")  ("def")}"#),
            Ok((
                "",
                InlineDataFull::new(
                    vec![Var::QMark("name".to_string())],
                    vec![
                        vec![DataBlockValue::RdfLiteral(RdfLiteral::literal("abc"))],
                        vec![DataBlockValue::RdfLiteral(RdfLiteral::literal("def"))]
                    ]
                )
            ))
        );
    }

    #[test]
    fn is_values_clause() {
        assert_eq!(
            values_clause(
                r#"values (?x ?y) {
  (:uri1 1)
  (:uri2 UNDEF)
}"#
            ),
            Ok((
                "",
                Some(DataBlock::InlineDataFull(InlineDataFull::new(
                    vec![Var::QMark("x".to_string()), Var::QMark("y".to_string())],
                    vec![
                        vec![
                            DataBlockValue::Iri(Iri::PrefixedName(PrefixedName::PnameLN {
                                pn_prefix: None,
                                pn_local: "uri1".to_string(),
                            },)),
                            DataBlockValue::NumericLiteral(NumericLiteral::Int(1))
                        ],
                        vec![
                            DataBlockValue::Iri(Iri::PrefixedName(PrefixedName::PnameLN {
                                pn_prefix: None,
                                pn_local: "uri2".to_string(),
                            },)),
                            DataBlockValue::UnDef
                        ]
                    ]
                )))
            ))
        );
    }

    #[test]
    fn is_data_set_clause() {
        assert_eq!(
            data_set_clause("FROM    <http://example.org/foaf/aliceFoaf>"),
            Ok((
                "",
                DefaultOrNamedIri::Default(Iri::Iri(
                    "http://example.org/foaf/aliceFoaf".to_string()
                ))
            ))
        );

        assert_eq!(
            data_set_clause("FROM NAMED <http://example.org/bob>"),
            Ok((
                "",
                DefaultOrNamedIri::Named(Iri::Iri("http://example.org/bob".to_string()))
            ))
        );

        assert_eq!(
            data_set_clause("FROM NAMED :uri1"),
            Ok((
                "",
                DefaultOrNamedIri::Named(Iri::PrefixedName(PrefixedName::PnameLN {
                    pn_prefix: None,
                    pn_local: "uri1".to_string(),
                },))
            ))
        );
    }

}
