use crate::expression::{
    expression, expression_as_var, Expression, ExpressionAsVar, Iri, IriOrFunction, PrefixedName,
    VarOrExpressionAsVar,
};
use crate::node::{
    Collection, GraphNode, GraphTerm, ObjectList, PropertyList, RdfLiteral, RdfLiteralDescriptor,
    TriplesNode, VarOrTerm, VerbList,
};
use crate::query::{
    PrefixDecl, SolutionModifier, SparqlQuery, Var, VarOrIri, VarWildcard, WhereClause,
};
use nom::branch::alt;
use nom::bytes::complete::{escaped, tag, tag_no_case, take_while1, take_while_m_n};
use nom::character::complete::{anychar, char, digit1, none_of, one_of};

use nom::combinator::{complete, cond, cut, map, map_res, not, opt, peek};

use crate::data::{DataBlock, DataSetClause};
use crate::parser::{sp1, sp_enc, var};
use crate::triple::{arg_list, graph_term};
use nom::multi::{fold_many0, many1};
use nom::sequence::{delimited, pair, preceded, separated_pair, terminated, tuple};
use nom::{
    bytes::complete::take_while,
    character::{
        complete::{alpha1, alphanumeric1},
        is_alphabetic,
    },
    error::ErrorKind,
    AsChar, Err, IResult,
};

#[derive(Debug, Clone)]
pub struct SelectQuery {
    pub select_clause: SelectClause,
    pub dataset_clauses: Vec<DataSetClause>,
    pub where_clause: WhereClause,
    pub solution_modifier: SolutionModifier,
}

#[derive(Debug, Clone)]
pub struct SubSelect {
    pub select_clause: SelectClause,
    pub where_clause: WhereClause,
    pub solution_modifier: SolutionModifier,
    pub values_clause: Option<DataBlock>,
}

#[derive(Debug, Clone)]
pub struct SelectClause {
    pub modifier: Option<SelectModifier>,
    pub vars: SelectVars,
}

#[derive(Debug, Clone)]
pub enum SelectVars {
    VarOrExpressionAsVar(Vec<VarOrExpressionAsVar>),
    All,
}

#[derive(Debug, Clone)]
pub enum SelectModifier {
    Distinct,
    Reduced,
}

pub(crate) fn select_clause(i: &str) -> IResult<&str, SelectClause> {
    map(
        tuple((
            tag_no_case("select"),
            opt(preceded(sp1, select_modifier)),
            preceded(sp1, select_vars),
        )),
        |(_, modifier, vars)| SelectClause { modifier, vars },
    )(i)
}

pub(crate) fn select_vars(i: &str) -> IResult<&str, SelectVars> {
    alt((
        map(char('*'), |_| SelectVars::All),
        map(
            many1(var_or_expression_as_var),
            SelectVars::VarOrExpressionAsVar,
        ),
    ))(i)
}

pub(crate) fn var_or_expression_as_var(i: &str) -> IResult<&str, VarOrExpressionAsVar> {
    alt((
        map(var, VarOrExpressionAsVar::Var),
        map(expression_as_var, VarOrExpressionAsVar::ExpressionAsVar),
    ))(i)
}

pub(crate) fn select_modifier(i: &str) -> IResult<&str, SelectModifier> {
    alt((
        map(tag_no_case("distinct"), |_| SelectModifier::Distinct),
        map(tag_no_case("reduced"), |_| SelectModifier::Reduced),
    ))(i)
}
