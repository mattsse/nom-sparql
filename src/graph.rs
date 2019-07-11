use crate::parser::{default_or_named_iri, iri, preceded_tag, sp, sp1};

use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, tag_no_case, take_while1, take_while_m_n},
    character::is_digit,
    combinator::map_res,
    sequence::{preceded, terminated},
    IResult,
};

use crate::data::{datablock, DataBlock};
use crate::expression::{DefaultOrNamedIri, Iri};
use crate::node::GroupGraphPattern;
use crate::triple::{quads_pattern, Quads};
use nom::combinator::{map, opt};
use nom::sequence::separated_pair;

pub(crate) fn group_graph_pattern(i: &str) -> IResult<&str, GroupGraphPattern> {
    unimplemented!()
}
