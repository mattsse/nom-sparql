use std::fmt;

use crate::ask::AskQuery;
use crate::clauses::SolutionModifier;
use crate::construct::ConstructQuery;
use crate::data::{DataBlock, DataSetClause};
use crate::describe::DescribeQuery;
use crate::expression::Iri;
use crate::graph::GroupGraphPattern;
use crate::select::SelectQuery;

#[derive(Debug, Clone)]
pub struct SparqlQueryStatement {
    pub prologue: Prologue,
    pub query: SparqlQuery,
    pub values: Option<DataBlock>,
}

#[derive(Debug, Clone)]
pub enum SparqlQuery {
    Ask(AskQuery),
    Select(SelectQuery),
    Describe(DescribeQuery),
    Construct(ConstructQuery),
}

impl fmt::Display for SparqlQuery {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!()
    }
}

#[derive(Debug, Clone)]
pub struct PrefixDecl {
    pub pname_ns: Option<String>,
    pub iri_ref: String,
}

#[derive(Debug, Clone)]
pub struct Prologue(pub Vec<BaseOrPrefixDecl>);

#[derive(Debug, Clone)]
pub enum BaseOrPrefixDecl {
    Base(String),
    Prefix(PrefixDecl),
}

#[derive(Debug, Clone)]
pub enum VarOrIri {
    Var(Var),
    Iri(Iri),
}

#[derive(Debug, Clone)]
pub enum Var {
    Var1(String),
    Var2(String),
}
