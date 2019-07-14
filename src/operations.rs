use nom::combinator::{map, opt};
use nom::sequence::{delimited, pair, tuple};
use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, tag_no_case, take_while1, take_while_m_n},
    character::is_digit,
    combinator::map_res,
    sequence::{preceded, terminated},
    IResult,
};

use crate::clauses::{delete_clause, insert_clause, using_clause};
use crate::expression::{DefaultOrNamedIri, Iri};
use crate::graph::{
    graph_or_default, graph_ref, graph_ref_all, group_graph_pattern, GraphOrDefault, GraphRefAll,
    GroupGraphPattern,
};
use crate::literal::silent;
use crate::parser::{iri, preceded_tag1, sp, sp1, sp_enc, sp_enc1};
use crate::triple::{quad_data, Quads};
use nom::multi::separated_list;

#[derive(Debug, Clone, Eq, PartialEq, new)]
pub struct LoadStatement {
    pub iri: Iri,
    pub silent: bool,
    pub graph_ref: Option<Iri>,
}

#[derive(Debug, Clone, Eq, PartialEq, new)]
pub struct ClearStatement {
    pub silent: bool,
    pub graph_ref_all: GraphRefAll,
}

#[derive(Debug, Clone, Eq, PartialEq, new)]
pub struct DropStatement {
    pub silent: bool,
    pub graph_ref_all: GraphRefAll,
}

#[derive(Debug, Clone, Eq, PartialEq, new)]
pub struct CreateStatement {
    pub silent: bool,
    pub graph_ref: Iri,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AddStatement {
    pub silent: bool,
    pub from: GraphOrDefault,
    pub to: GraphOrDefault,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MoveStatement {
    pub silent: bool,
    pub from: GraphOrDefault,
    pub to: GraphOrDefault,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CopyStatement {
    pub silent: bool,
    pub from: GraphOrDefault,
    pub to: GraphOrDefault,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ModifyStatement {
    pub iri: Option<Iri>,
    pub delete_insert: DeleteInsert,
    pub using_clauses: Vec<DefaultOrNamedIri>,
    pub where_group_graph: GroupGraphPattern,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum DeleteInsert {
    DeleteInsert {
        delete: Quads,
        insert: Option<Quads>,
    },
    Insert(Quads),
}

pub(crate) fn add_stmt(i: &str) -> IResult<&str, AddStatement> {
    map(
        pair(pair(tag_no_case("add"), sp1), silent_from_to),
        |(_, (silent, from, to))| AddStatement { silent, from, to },
    )(i)
}

pub(crate) fn modify_stmt(i: &str) -> IResult<&str, ModifyStatement> {
    map(
        tuple((
            opt(delimited(tag_no_case("where"), iri, sp1)),
            terminated(delete_insert, sp),
            separated_list(sp, using_clause),
            sp_enc1(tag_no_case("where")),
            group_graph_pattern,
        )),
        |(iri, delete_insert, using_clauses, _, where_group_graph)| ModifyStatement {
            iri,
            delete_insert,
            using_clauses,
            where_group_graph,
        },
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
        map(opt(terminated(silent, sp1)), Option::unwrap_or_default),
        graph_or_default,
        preceded(sp_enc(tag_no_case("to")), graph_or_default),
    ))(i)
}

pub(crate) fn create_stmt(i: &str) -> IResult<&str, CreateStatement> {
    map(
        tuple((
            pair(tag_no_case("create"), sp1),
            map(opt(terminated(silent, sp1)), Option::unwrap_or_default),
            graph_ref,
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
        map(opt(terminated(silent, sp1)), Option::unwrap_or_default),
        graph_ref_all,
    )(i)
}

pub(crate) fn load_stmt(i: &str) -> IResult<&str, LoadStatement> {
    map(
        tuple((
            pair(tag_no_case("load"), sp1),
            map(opt(terminated(silent, sp1)), Option::unwrap_or_default),
            iri,
            opt(preceded(sp_enc(tag_no_case("into")), graph_ref)),
        )),
        |(_, silent, iri, graph_ref)| LoadStatement {
            iri,
            silent,
            graph_ref,
        },
    )(i)
}

pub(crate) fn insert_data(i: &str) -> IResult<&str, Quads> {
    preceded_tag1("insert", preceded_tag1("data", quad_data))(i)
}

pub(crate) fn delete_data(i: &str) -> IResult<&str, Quads> {
    preceded_tag1("delete", preceded_tag1("data", quad_data))(i)
}

pub(crate) fn delete_where_data(i: &str) -> IResult<&str, Quads> {
    preceded_tag1("delete", preceded_tag1("where", quad_data))(i)
}

pub(crate) fn delete_insert(i: &str) -> IResult<&str, DeleteInsert> {
    alt((
        map(
            pair(delete_clause, opt(preceded(sp, insert_clause))),
            |(delete, insert)| DeleteInsert::DeleteInsert { delete, insert },
        ),
        map(insert_clause, DeleteInsert::Insert),
    ))(i)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expression::PrefixedName;

    #[test]
    fn is_drop_stmt() {
        assert_eq!(
            drop_stmt("drop  graph :uri1"),
            Ok((
                "",
                DropStatement::new(
                    false,
                    GraphRefAll::GraphRef(Iri::PrefixedName(PrefixedName::PnameLN {
                        pn_prefix: None,
                        pn_local: "uri1".to_string(),
                    },))
                )
            ))
        );

        assert_eq!(
            drop_stmt("drop silent graph <http://example.org/foaf/aliceFoaf>"),
            Ok((
                "",
                DropStatement::new(
                    true,
                    GraphRefAll::GraphRef(Iri::Iri(
                        "http://example.org/foaf/aliceFoaf".to_string()
                    ))
                )
            ))
        );
    }

    #[test]
    fn is_create_stmt() {
        assert_eq!(
            create_stmt("create silent graph <http://example.org/foaf/aliceFoaf>"),
            Ok((
                "",
                CreateStatement::new(
                    true,
                    Iri::Iri("http://example.org/foaf/aliceFoaf".to_string())
                )
            ))
        );

        assert_eq!(
            create_stmt("create   graph   :uri1"),
            Ok((
                "",
                CreateStatement::new(
                    false,
                    Iri::PrefixedName(PrefixedName::PnameLN {
                        pn_prefix: None,
                        pn_local: "uri1".to_string(),
                    },)
                )
            ))
        )
    }
}
