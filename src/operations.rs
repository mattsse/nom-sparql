use crate::parser::{iri, preceded_tag, sp, sp1};

use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, tag_no_case, take_while1, take_while_m_n},
    character::is_digit,
    combinator::map_res,
    sequence::{preceded, terminated},
    IResult,
};

use crate::expression::Iri;
use crate::literal::silent;
use crate::node::{graph_ref, GroupGraphPattern};
use crate::query::LimitOffsetClause;
use crate::triple::{quad_data, quads_pattern, Quads};
use nom::combinator::{map, opt};
use nom::sequence::{pair, separated_pair, tuple};

#[derive(Debug, Clone)]
pub struct Update {
    pub prologue: String,
    pub update_stmt: Option<UpdateStatement>,
    pub further: Option<Box<Update>>,
}

#[derive(Debug, Clone)]
pub enum UpdateStatement {
    Load(LoadStatement),
    Clear,
    Drop,
    Add,
    Move,
    Copy,
    Create,
    InsertData(Quads),
    DeleteData(Quads),
    DeleteWhere(Quads),
    Modify(ModifyStatement),
}

#[derive(Debug, Clone)]
pub struct LoadStatement {
    pub iri: Iri,
    pub silent: bool,
    pub graph_ref: Option<Iri>,
}

pub(crate) fn load_stmt(i: &str) -> IResult<&str, LoadStatement> {
    map(
        tuple((
            pair(tag_no_case("load"), sp1),
            map(opt(terminated(silent, sp1)), |s| s.unwrap_or(false)),
            iri,
            opt(preceded(sp1, preceded_tag("into", graph_ref))),
        )),
        |(_, silent, iri, graph_ref)| LoadStatement {
            iri,
            silent,
            graph_ref,
        },
    )(i)
}

#[derive(Debug, Clone)]
pub struct ModifyStatement {
    pub iri: Option<Iri>,
    pub delete_insert: Option<DeleteInsert>,
    pub using_clauses: Vec<Iri>,
    pub where_group_graph: GroupGraphPattern,
}

#[derive(Debug, Clone)]
pub enum DeleteInsert {
    DeleteInsert {
        delete: Quads,
        insert: Option<Quads>,
    },
    Insert(Quads),
}

pub(crate) fn insert_data(i: &str) -> IResult<&str, Quads> {
    preceded_tag("insert", preceded_tag("data", quad_data))(i)
}
pub(crate) fn delete_data(i: &str) -> IResult<&str, Quads> {
    preceded_tag("delete", preceded_tag("data", quad_data))(i)
}
pub(crate) fn delete_where_data(i: &str) -> IResult<&str, Quads> {
    preceded_tag("delete", preceded_tag("where", quad_data))(i)
}
