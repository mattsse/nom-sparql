use crate::clauses::{solution_modifier, where_clause, SolutionModifier};
use crate::data::{data_set_clause, DataSetClause};
use crate::graph::GroupGraphPattern;
use crate::node::TriplesSameSubject;
use crate::parser::{preceded_tag1, sp, sp1, sp_enc, sp_enc1, var_or_iri};
use crate::query::VarOrIri;
use crate::triple::{triples_same_subject, triples_template, TriplesTemplate};
use nom::{
    branch::alt,
    bytes::complete::tag_no_case,
    character::complete::char,
    combinator::{map, opt},
    multi::many1,
    multi::{separated_list, separated_nonempty_list},
    sequence::{delimited, preceded},
    sequence::{terminated, tuple},
    IResult,
};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ConstructQuery {
    SubConstruct(SubConstruct),
    SubTriples(SubTriples),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SubConstruct {
    pub construct_template: ConstructTemplate,
    pub dataset_clauses: Vec<DataSetClause>,
    pub where_clause: GroupGraphPattern,
    pub solution_modifier: SolutionModifier,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SubTriples {
    pub dataset_clauses: Vec<DataSetClause>,
    pub triples_template: Option<TriplesTemplate>,
    pub solution_modifier: SolutionModifier,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ConstructTemplate(pub Vec<TriplesSameSubject>);

pub fn sub_triples(i: &str) -> IResult<&str, SubTriples> {
    map(
        tuple((
            terminated(separated_list(sp, data_set_clause), sp),
            terminated(tag_no_case("where"), sp1),
            delimited(char('{'), sp_enc(opt(triples_template)), char('}')),
            preceded(sp, solution_modifier),
        )),
        |(dataset_clauses, _, triples_template, solution_modifier)| SubTriples {
            dataset_clauses,
            triples_template,
            solution_modifier,
        },
    )(i)
}

pub fn sub_construct(i: &str) -> IResult<&str, SubConstruct> {
    map(
        tuple((
            terminated(construct_template, sp1),
            terminated(separated_list(sp, data_set_clause), sp),
            terminated(where_clause, sp),
            solution_modifier,
        )),
        |(construct_template, dataset_clauses, where_clause, solution_modifier)| SubConstruct {
            construct_template,
            dataset_clauses,
            where_clause,
            solution_modifier,
        },
    )(i)
}

pub fn construct_query(i: &str) -> IResult<&str, ConstructQuery> {
    preceded_tag1(
        "construct",
        alt((
            map(sub_construct, ConstructQuery::SubConstruct),
            map(sub_triples, ConstructQuery::SubTriples),
        )),
    )(i)
}

pub(crate) fn construct_template(i: &str) -> IResult<&str, ConstructTemplate> {
    map(
        delimited(
            terminated(char('{'), sp),
            separated_list(many1(sp_enc(char('.'))), triples_same_subject),
            preceded(sp, char('}')),
        ),
        ConstructTemplate,
    )(i)
}
