use crate::node::RdfLiteral;
use crate::query::Var;

use crate::arithmetic::{ConditionalOrExpression, NumericExpression};
use crate::expression::{ArgList, Iri};
use crate::literal::NumericLiteral;
use crate::parser::{preceded_tag, sp_enc, var};
use nom::bytes::complete::tag_no_case;
use nom::character::complete::char;
use nom::combinator::map;
use nom::sequence::{delimited, separated_pair};
use nom::IResult;

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub iri_ref: Iri,
    pub args: ArgList,
}
