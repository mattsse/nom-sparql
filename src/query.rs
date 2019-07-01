use crate::expression::{Constraint, IriRef, OrderExpression};
use crate::node::GroupGraphPattern;
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
pub struct AskQuery {
    pub dataset_clauses: Vec<DataSetClause>,
    pub where_clause: WhereClause,
}

#[derive(Debug, Clone)]
pub struct SelectQuery {
    pub modifier: Option<SelectModifier>,
    pub var_wildcard: VarWildcard,
    pub dataset_clauses: Vec<DataSetClause>,
    pub where_clause: WhereClause,
    pub solution_modifier: SolutionModifier,
}

#[derive(Debug, Clone)]
pub struct DescribeQuery {
    pub dataset_clauses: Vec<DataSetClause>,
    pub var_iri_ref_wildcard: VarIriRefWildcard,
    pub where_clause: Option<WhereClause>,
    pub solution_modifier: SolutionModifier,
}

#[derive(Debug, Clone)]
pub enum VarIriRefWildcard {
    VarIriRef(Vec<VarOrIriRef>),
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
    pub where_clause: WhereClause,
    pub solution_modifier: SolutionModifier,
}

#[derive(Debug, Clone)]
pub enum SelectModifier {
    Distinct,
    Reduced,
}

#[derive(Debug, Clone)]
pub struct SolutionModifier {
    pub order_by: Option<OrderClause>,
    pub limit_offset_clause: Option<LimitOffsetClause>,
}

#[derive(Debug, Clone)]
pub struct LimitClause {
    pub limit: usize,
}

#[derive(Debug, Clone)]
pub struct ConstructTemplate {}

#[derive(Debug, Clone)]
pub struct OrderClause {
    pub condition: Vec<OrderCondition>,
}

#[derive(Debug, Clone)]
pub enum OrderCondition {
    Order(Box<OrderExpression>),
    Constraint(Box<Constraint>),
    Var(Var),
}

#[derive(Debug, Clone)]
pub struct OffsetClause {
    pub offset: usize,
}

#[derive(Debug, Clone)]
pub struct DataSetClause {
    pub graph_clause: GraphClause,
}

#[derive(Debug, Clone)]
pub enum GraphClause {
    Default(IriRef),
    Named(IriRef),
}

#[derive(Debug, Clone)]
pub enum LimitOffsetClause {
    LimitClauseOffsetClause {
        limit_clause: LimitClause,
        offset_clause: Option<OffsetClause>,
    },
    OffsetClauseLimitClause {
        offset_clause: OffsetClause,
        limit_clause: Option<LimitClause>,
    },
}

#[derive(Debug, Clone)]
pub struct WhereClause {
    pub group_graph_pattern: GroupGraphPattern,
}

#[derive(Debug, Clone)]
pub struct Prolog {}

#[derive(Debug, Clone)]
pub enum VarOrIriRef {
    Var(Var),
    IriRef(IriRef),
}

#[derive(Debug, Clone)]
pub enum Var {
    Var1(String),
    Var2(String),
}
