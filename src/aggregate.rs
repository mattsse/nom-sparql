use crate::expression::{ArgList, Expression, Iri};

#[derive(Debug, Clone)]
pub enum Aggregate {
    Count(Count),
    Sum(AggregateModifier),
    Min(AggregateModifier),
    Max(AggregateModifier),
    Avg(AggregateModifier),
    Sample(AggregateModifier),
    GroupConcat(GroupConcat),
}

impl Aggregate {
    pub fn is_distinct(&self) -> bool {
        match self {
            Aggregate::Sum(a)
            | Aggregate::Min(a)
            | Aggregate::Max(a)
            | Aggregate::Avg(a)
            | Aggregate::Sample(a) => a.distinct,
            Aggregate::Count(c) => c.distinct,
            Aggregate::GroupConcat(c) => c.modifier.distinct,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Count {
    pub distinct: bool,
    pub target: CountTarget,
}

#[derive(Debug, Clone)]
pub enum CountTarget {
    All,
    Expr(Box<Expression>),
}

#[derive(Debug, Clone)]
pub struct GroupConcat {
    pub modifier: AggregateModifier,
    pub separator: Option<String>,
}

#[derive(Debug, Clone)]
pub struct AggregateModifier {
    pub distinct: bool,
    pub expression: Box<Expression>,
}
