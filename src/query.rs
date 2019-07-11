use crate::clauses::SolutionModifier;
use crate::data::DataSetClause;
use crate::expression::{Constraint, Iri};
use crate::node::GroupGraphPattern;
use crate::select::SelectQuery;
use std::fmt;

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
pub struct AskQuery {
    pub dataset_clauses: Vec<DataSetClause>,
    pub where_clause: GroupGraphPattern,
}

#[derive(Debug, Clone)]
pub struct DescribeQuery {
    pub dataset_clauses: Vec<DataSetClause>,
    pub var_iri_ref_wildcard: VarIriRefWildcard,
    pub where_clause: Option<GroupGraphPattern>,
    pub solution_modifier: SolutionModifier,
}

#[derive(Debug, Clone)]
pub enum VarIriRefWildcard {
    VarIriRef(Vec<VarOrIri>),
    WildCard,
}

#[derive(Debug, Clone)]
pub enum VarWildcard {
    Var(Vec<Var>),
    WildCard,
}

#[derive(Debug, Clone)]
pub struct ConstructQuery {
    pub construct_template: ConstructTemplate,
    pub dataset_clauses: Vec<DataSetClause>,
    pub where_clause: GroupGraphPattern,
    pub solution_modifier: SolutionModifier,
}

#[derive(Debug, Clone)]
pub struct ConstructTemplate {}

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
