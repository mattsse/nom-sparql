use std::fmt;

use crate::data::DataBlock;
use crate::prologue::Prologue;
use crate::query::{
    ask::AskQuery, construct::ConstructQuery, describe::DescribeQuery, select::SelectQuery,
};

pub mod ask;
pub mod construct;
pub mod describe;
pub mod select;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SparqlQueryStatement {
    pub prologue: Prologue,
    pub query: SparqlQuery,
    pub values: Option<DataBlock>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
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
