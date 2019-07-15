use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, tag_no_case, take_while1, take_while_m_n},
    character::complete::char,
    character::is_digit,
    combinator::map_res,
    combinator::{map, opt},
    multi::{many0, many1, separated_nonempty_list},
    sequence::{delimited, pair, separated_pair, tuple},
    sequence::{preceded, terminated},
    IResult,
};

use crate::{
    expression::ArgList,
    graph::graph_node,
    node::{Collection, ObjectList, PropertyList, TriplesNode},
    path::{triples_same_subject_path, TriplesSameSubjectPath},
    terminals::{bracketted, sp, sp_enc, sp_sep, sp_sep1},
    triple::{triples_template, TriplesTemplate},
    var::{var_or_iri, var_or_term, VarOrIri, VarOrTerm, VerbList},
};

#[derive(Debug, Clone, Eq, PartialEq, new)]
pub struct Quads {
    pub first_triple: Option<TriplesTemplate>,
    pub entries: Vec<QuadsEntry>,
}

#[derive(Debug, Clone, Eq, PartialEq, new)]
pub struct QuadsEntry {
    pub quads_not_triples: QuadsNotTriples,
    pub triples_template: Option<TriplesTemplate>,
}

#[derive(Debug, Clone, Eq, PartialEq, new)]
pub struct QuadsNotTriples {
    pub var_or_iri: VarOrIri,
    pub triples_template: Option<TriplesTemplate>,
}

pub(crate) fn quads_pattern(i: &str) -> IResult<&str, Quads> {
    delimited(terminated(tag("{"), sp), quads, terminated(sp, tag("}")))(i)
}

pub(crate) fn quad_data(i: &str) -> IResult<&str, Quads> {
    quads_pattern(i)
}

pub(crate) fn quads(i: &str) -> IResult<&str, Quads> {
    map(
        pair(opt(terminated(triples_template, sp)), many0(quads_entry)),
        |(first_triples, entries)| Quads {
            first_triple: first_triples,
            entries,
        },
    )(i)
}

pub(crate) fn quads_not_triples(i: &str) -> IResult<&str, QuadsNotTriples> {
    map(
        tuple((
            tag_no_case("graph"),
            sp_enc(var_or_iri),
            delimited(char('{'), sp_enc(opt(triples_template)), char('}')),
        )),
        |(_, var_or_iri, triples_template)| QuadsNotTriples {
            var_or_iri,
            triples_template,
        },
    )(i)
}

pub(crate) fn quads_entry(i: &str) -> IResult<&str, QuadsEntry> {
    map(
        separated_pair(quads_not_triples, opt(char('.')), opt(triples_template)),
        |(quads_not_triples, triples_template)| QuadsEntry {
            quads_not_triples,
            triples_template,
        },
    )(i)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expression::{Iri, PrefixedName};
    use crate::graph::{GraphNode, GraphTerm};
    use crate::triple::TriplesSameSubject;
    use crate::var::{Var, Verb};

    #[test]
    fn is_quads_not_triples() {
        let _nil = TriplesSameSubject::Node {
            triples_node: TriplesNode::Collection(Collection(vec![
                GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::BooleanLiteral(true))),
                GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::Nil)),
            ])),
            property_list: None,
        };

        assert_eq!(
            quads_not_triples("graph ?name {}"),
            Ok((
                "",
                QuadsNotTriples::new(VarOrIri::Var(Var::QMark("name".to_string())), None)
            ))
        );

        assert_eq!(
            quads_not_triples("graph ?name { (true ()) }"),
            Ok((
                "",
                QuadsNotTriples::new(
                    VarOrIri::Var(Var::QMark("name".to_string())),
                    Some(vec![TriplesSameSubject::Node {
                        triples_node: TriplesNode::Collection(Collection(vec![
                            GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::BooleanLiteral(true))),
                            GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::Nil)),
                        ])),
                        property_list: None,
                    }])
                )
            ))
        );

        assert_eq!(
            quads_not_triples("GRAPH ?g { ?x foaf:mbox ?mbox }"),
            Ok((
                "",
                QuadsNotTriples::new(
                    VarOrIri::Var(Var::QMark("g".to_string())),
                    Some(vec![TriplesSameSubject::Term {
                        var_or_term: VarOrTerm::Var(Var::QMark("x".to_string())),
                        property_list: PropertyList(vec![VerbList::new(
                            Verb::VarOrIri(VarOrIri::Iri(Iri::PrefixedName(
                                PrefixedName::PnameLN {
                                    pn_prefix: Some("foaf".to_string()),
                                    pn_local: "mbox".to_string(),
                                }
                            ))),
                            ObjectList(vec![GraphNode::VarOrTerm(VarOrTerm::Var(Var::QMark(
                                "mbox".to_string(),
                            )))]),
                        )]),
                    }])
                )
            ))
        );
    }

    #[test]
    fn is_quads_pattern() {
        assert_eq!(
            quads_pattern(
                r#"{
   ?g dc:publisher ?who .
   GRAPH ?g { ?x foaf:mbox ?mbox }
}"#
            ),
            Ok((
                "",
                Quads::new(
                    Some(vec![TriplesSameSubject::Term {
                        var_or_term: VarOrTerm::Var(Var::QMark("g".to_string())),
                        property_list: PropertyList(vec![VerbList::new(
                            Verb::VarOrIri(VarOrIri::Iri(Iri::PrefixedName(
                                PrefixedName::PnameLN {
                                    pn_prefix: Some("dc".to_string()),
                                    pn_local: "publisher".to_string(),
                                }
                            ))),
                            ObjectList(vec![GraphNode::VarOrTerm(VarOrTerm::Var(Var::QMark(
                                "who".to_string(),
                            )))]),
                        )]),
                    }]),
                    vec![QuadsEntry::new(
                        QuadsNotTriples::new(
                            VarOrIri::Var(Var::QMark("g".to_string())),
                            Some(vec![TriplesSameSubject::Term {
                                var_or_term: VarOrTerm::Var(Var::QMark("x".to_string())),
                                property_list: PropertyList(vec![VerbList::new(
                                    Verb::VarOrIri(VarOrIri::Iri(Iri::PrefixedName(
                                        PrefixedName::PnameLN {
                                            pn_prefix: Some("foaf".to_string()),
                                            pn_local: "mbox".to_string(),
                                        }
                                    ))),
                                    ObjectList(vec![GraphNode::VarOrTerm(VarOrTerm::Var(
                                        Var::QMark("mbox".to_string(),)
                                    ))]),
                                )]),
                            }])
                        ),
                        None
                    )]
                )
            ))
        );
    }
}
