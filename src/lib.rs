//#![deny(missing_docs)]

#![allow(unused)]

#[macro_use]
extern crate derive_new;

/// https://www.obitko.com/tutorials/ontologies-semantic-web/rdf-query-language-sparql.html
pub mod aggregate;
pub mod arithmetic;
pub mod call;
pub mod clauses;
pub mod data;
pub mod expression;
pub mod graph;
pub mod group;
pub mod literal;
pub mod node;
pub mod operations;
pub mod order;
pub mod parser;
pub mod path;
pub mod prologue;
pub mod quads;
pub mod query;
pub mod read;
pub mod terminals;
pub mod triple;
pub mod update;
pub mod var;
pub mod write;

pub mod mimes {}
