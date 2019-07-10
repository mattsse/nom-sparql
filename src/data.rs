use crate::expression::Iri;
use crate::literal::{boolean, numeric_literal, NumericLiteral};
use crate::node::RdfLiteral;
use crate::parser::{iri, nil, rdf_literal, sp, sp_enc, var};
use crate::query::Var;
use nom::branch::alt;
use nom::bytes::complete::tag_no_case;
use nom::character::complete::char;
use nom::combinator::map;
use nom::multi::many0;
use nom::sequence::{delimited, pair, preceded, tuple};
use nom::IResult;

#[derive(Debug, Clone)]
pub enum DataBlock {
    InlineDataOneVar(InlineDataOneVar),
    InlineDataFull(InlineDataFull),
}

#[derive(Debug, Clone)]
pub struct InlineDataOneVar {
    pub var: Var,
    pub values: Vec<DataBlockValue>,
}

#[derive(Debug, Clone)]
pub struct InlineDataFull {
    pub vars: Vec<Var>,
    pub data_block_values: Vec<Vec<DataBlockValue>>,
}

#[derive(Debug, Clone)]
pub enum DataBlockValue {
    Iri(Iri),
    RdfLiteral(RdfLiteral),
    NumericLiteral(NumericLiteral),
    BooleanLiteral(bool),
    UnDef,
}

pub(crate) fn datablock(i: &str) -> IResult<&str, DataBlock> {
    alt((
        map(inline_data_one_var, DataBlock::InlineDataOneVar),
        map(inline_data_full, DataBlock::InlineDataFull),
    ))(i)
}

pub(crate) fn inline_data_full(i: &str) -> IResult<&str, InlineDataFull> {
    map(
        tuple((
            alt((
                map(nil, |_| Vec::new()),
                delimited(char('('), many0(sp_enc(var)), char(')')),
            )),
            delimited(
                sp_enc(char('{')),
                many0(alt((
                    map(sp_enc(nil), |_| Vec::new()),
                    delimited(
                        sp_enc(char('(')),
                        many0(sp_enc(datablock_value)),
                        sp_enc(char(')')),
                    ),
                ))),
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
