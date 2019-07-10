use crate::parser::{iri, preceded_tag, sp, sp1, sp_enc};

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
use crate::node::{
    graph_or_default, graph_ref, graph_ref_all, GraphOrDefault, GraphRefAll, GroupGraphPattern,
};
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
    Clear(ClearStatement),
    Drop(DropStatement),
    Create(CreateStatement),
    Add(AddStatement),
    Move(MoveStatement),
    Copy(CopyStatement),
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

#[derive(Debug, Clone)]
pub struct ClearStatement {
    pub silent: bool,
    pub graph_ref_all: GraphRefAll,
}

#[derive(Debug, Clone)]
pub struct DropStatement {
    pub silent: bool,
    pub graph_ref_all: GraphRefAll,
}

#[derive(Debug, Clone)]
pub struct CreateStatement {
    pub silent: bool,
    pub graph_ref: Iri,
}

#[derive(Debug, Clone)]
pub struct AddStatement {
    pub silent: bool,
    pub from: GraphOrDefault,
    pub to: GraphOrDefault,
}

#[derive(Debug, Clone)]
pub struct MoveStatement {
    pub silent: bool,
    pub from: GraphOrDefault,
    pub to: GraphOrDefault,
}

#[derive(Debug, Clone)]
pub struct CopyStatement {
    pub silent: bool,
    pub from: GraphOrDefault,
    pub to: GraphOrDefault,
}

pub(crate) fn add_stmt(i: &str) -> IResult<&str, AddStatement> {
    map(
        pair(pair(tag_no_case("add"), sp1), silent_from_to),
        |(_, (silent, from, to))| AddStatement { silent, from, to },
    )(i)
}
pub(crate) fn move_stmt(i: &str) -> IResult<&str, MoveStatement> {
    map(
        pair(pair(tag_no_case("move"), sp1), silent_from_to),
        |(_, (silent, from, to))| MoveStatement { silent, from, to },
    )(i)
}

pub(crate) fn copy_stmt(i: &str) -> IResult<&str, CopyStatement> {
    map(
        pair(pair(tag_no_case("copy"), sp1), silent_from_to),
        |(_, (silent, from, to))| CopyStatement { silent, from, to },
    )(i)
}

pub(crate) fn silent_from_to(i: &str) -> IResult<&str, (bool, GraphOrDefault, GraphOrDefault)> {
    tuple((
        map(opt(silent), Option::unwrap_or_default),
        preceded(sp1, graph_or_default),
        preceded(sp_enc(tag_no_case("to")), graph_or_default),
    ))(i)
}

pub(crate) fn create_stmt(i: &str) -> IResult<&str, CreateStatement> {
    map(
        tuple((
            pair(tag_no_case("create"), sp1),
            map(opt(silent), Option::unwrap_or_default),
            preceded(sp1, graph_ref),
        )),
        |(_, silent, graph_ref)| CreateStatement { silent, graph_ref },
    )(i)
}

pub(crate) fn drop_stmt(i: &str) -> IResult<&str, DropStatement> {
    map(
        pair(pair(tag_no_case("drop"), sp1), silent_graph_ref_all),
        |(_, (silent, graph_ref_all))| DropStatement {
            silent,
            graph_ref_all,
        },
    )(i)
}

pub(crate) fn clear_stmt(i: &str) -> IResult<&str, ClearStatement> {
    map(
        pair(pair(tag_no_case("clear"), sp1), silent_graph_ref_all),
        |(_, (silent, graph_ref_all))| ClearStatement {
            silent,
            graph_ref_all,
        },
    )(i)
}

pub(crate) fn silent_graph_ref_all(i: &str) -> IResult<&str, (bool, GraphRefAll)> {
    pair(
        map(opt(silent), Option::unwrap_or_default),
        preceded(sp1, graph_ref_all),
    )(i)
}

pub(crate) fn load_stmt(i: &str) -> IResult<&str, LoadStatement> {
    map(
        tuple((
            pair(tag_no_case("load"), sp1),
            map(opt(silent), Option::unwrap_or_default),
            preceded(sp1, iri),
            opt(preceded(sp_enc(tag_no_case("into")), graph_ref)),
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
