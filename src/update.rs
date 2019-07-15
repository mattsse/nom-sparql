use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, tag_no_case, take_while1, take_while_m_n},
    character::complete::char,
    character::is_digit,
    combinator::map_res,
    combinator::{map, opt},
    sequence::{pair, tuple},
    sequence::{preceded, terminated},
    IResult,
};

use crate::expression::Iri;
use crate::graph::{
    graph_or_default, graph_ref, graph_ref_all, GraphOrDefault, GraphRefAll, GroupGraphPattern,
};
use crate::literal::silent;
use crate::operations::{
    clear_stmt, copy_stmt, create_stmt, delete_data, delete_where_data, drop_stmt, insert_data,
    load_stmt, modify_stmt, AddStatement, ClearStatement, CopyStatement, CreateStatement,
    DropStatement, LoadStatement, ModifyStatement, MoveStatement,
};
use crate::parser::{iri, preceded_tag1, prologue, sp, sp1, sp_enc};
use crate::quads::{quad_data, Quads};
use crate::query::Prologue;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Update {
    pub prologue: Prologue,
    pub update_stmt: Option<UpdateStatement>,
    pub inner_update: Option<Box<Update>>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
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
    Modify(Box<ModifyStatement>),
}

pub(crate) fn update(i: &str) -> IResult<&str, Update> {
    map(
        tuple((
            prologue,
            opt(pair(
                preceded(sp, update_stmt),
                opt(preceded(sp_enc(char(';')), map(update, Box::new))),
            )),
        )),
        |(prologue, opts)| {
            if let Some((update_stmt, inner_update)) = opts {
                Update {
                    prologue,
                    update_stmt: Some(update_stmt),
                    inner_update,
                }
            } else {
                Update {
                    prologue,
                    update_stmt: None,
                    inner_update: None,
                }
            }
        },
    )(i)
}

pub(crate) fn update_stmt(i: &str) -> IResult<&str, UpdateStatement> {
    alt((
        map(load_stmt, UpdateStatement::Load),
        map(clear_stmt, UpdateStatement::Clear),
        map(drop_stmt, UpdateStatement::Drop),
        map(create_stmt, UpdateStatement::Create),
        map(copy_stmt, UpdateStatement::Copy),
        map(insert_data, UpdateStatement::InsertData),
        map(delete_data, UpdateStatement::DeleteData),
        map(delete_where_data, UpdateStatement::DeleteWhere),
        map(modify_stmt, |modify| {
            UpdateStatement::Modify(Box::new(modify))
        }),
    ))(i)
}
