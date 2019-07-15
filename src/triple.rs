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
    node::{Collection, ObjectList, PropertyList, TriplesNode, VarOrTerm, VerbList},
    parser::{bracketted, sp, sp_enc, sp_sep, sp_sep1, var_or_iri, var_or_term},
    path::{triples_same_subject_path, TriplesSameSubjectPath},
    query::VarOrIri,
};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TriplesSameSubject {
    Term {
        var_or_term: VarOrTerm,
        property_list: PropertyList,
    },
    Node {
        triples_node: TriplesNode,
        property_list: Option<PropertyList>,
    },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Verb {
    VarOrIri(VarOrIri),
    A,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ConstructTriples {
    pub first_triples: TriplesSameSubject,
    pub further_triples: Vec<TriplesSameSubject>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TriplesBlock(pub Vec<TriplesSameSubjectPath>);

pub type TriplesTemplate = Vec<TriplesSameSubject>;

pub(crate) fn blank_node_property_list(i: &str) -> IResult<&str, PropertyList> {
    delimited(
        terminated(char('['), sp),
        property_list_not_empty,
        preceded(sp, char(']')),
    )(i)
}

pub(crate) fn collection(i: &str) -> IResult<&str, Collection> {
    map(bracketted(many1(sp_enc(graph_node))), Collection)(i)
}

pub(crate) fn object_list(i: &str) -> IResult<&str, ObjectList> {
    map(
        separated_nonempty_list(sp_enc(tag(",")), graph_node),
        ObjectList,
    )(i)
}

pub(crate) fn property_list_not_empty(i: &str) -> IResult<&str, PropertyList> {
    map(
        terminated(
            separated_nonempty_list(
                sp_enc(many1(sp_enc(char(';')))),
                map(separated_pair(verb, sp, object_list), |(v, l)| {
                    VerbList::new(v, l)
                }),
            ),
            many0(preceded(sp, char(';'))),
        ),
        PropertyList,
    )(i)
}

#[inline]
pub(crate) fn property_list(i: &str) -> IResult<&str, Option<PropertyList>> {
    opt(property_list_not_empty)(i)
}

pub(crate) fn verb(i: &str) -> IResult<&str, Verb> {
    alt((
        map(var_or_iri, Verb::VarOrIri),
        map(tag_no_case("a"), |_| Verb::A),
    ))(i)
}

pub(crate) fn triples_node(i: &str) -> IResult<&str, TriplesNode> {
    alt((
        map(collection, TriplesNode::Collection),
        map(blank_node_property_list, TriplesNode::BlankNodePropertyList),
    ))(i)
}

pub(crate) fn triples_block(i: &str) -> IResult<&str, TriplesBlock> {
    map(
        separated_nonempty_list(many1(sp_enc(char('.'))), triples_same_subject_path),
        TriplesBlock,
    )(i)
}

pub(crate) fn triples_template(i: &str) -> IResult<&str, TriplesTemplate> {
    terminated(
        separated_nonempty_list(many1(sp_enc(char('.'))), triples_same_subject),
        opt(preceded(sp, char('.'))),
    )(i)
}

pub(crate) fn triples_same_subject(i: &str) -> IResult<&str, TriplesSameSubject> {
    alt((
        map(
            sp_sep(triples_node, property_list),
            |(triples_node, property_list)| TriplesSameSubject::Node {
                triples_node,
                property_list,
            },
        ),
        map(
            sp_sep1(var_or_term, property_list_not_empty),
            |(var_or_term, property_list)| TriplesSameSubject::Term {
                var_or_term,
                property_list,
            },
        ),
    ))(i)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expression::Iri;
    use crate::graph::{GraphNode, GraphTerm};
    use crate::query::Var;

    #[test]
    fn is_verb() {
        assert_eq!(verb("a"), Ok(("", Verb::A)));

        assert_eq!(
            verb("?name"),
            Ok((
                "",
                Verb::VarOrIri(VarOrIri::Var(Var::QMark("name".to_string())))
            ))
        );

        assert_eq!(
            verb("<http://example.org/foaf/aliceFoaf>"),
            Ok((
                "",
                Verb::VarOrIri(VarOrIri::Iri(Iri::Iri(
                    "http://example.org/foaf/aliceFoaf".to_string()
                )))
            ))
        );
    }

    #[test]
    fn is_triple_same_subject() {
        assert_eq!(
            triples_same_subject("() a true,()"),
            Ok((
                "",
                TriplesSameSubject::Term {
                    var_or_term: VarOrTerm::Term(GraphTerm::Nil),
                    property_list: PropertyList(vec![VerbList::new(
                        Verb::A,
                        ObjectList(vec![
                            GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::BooleanLiteral(true))),
                            GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::Nil)),
                        ]),
                    )])
                }
            ))
        );

        assert_eq!(
            triples_same_subject("(()())"),
            Ok((
                "",
                TriplesSameSubject::Node {
                    triples_node: TriplesNode::Collection(Collection(vec![
                        GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::Nil)),
                        GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::Nil))
                    ])),
                    property_list: None
                }
            ))
        );

        assert_eq!(
            triples_same_subject("[   a (),()\t] a false,()"),
            Ok((
                "",
                TriplesSameSubject::Node {
                    triples_node: TriplesNode::BlankNodePropertyList(PropertyList(vec![
                        VerbList::new(
                            Verb::A,
                            ObjectList(vec![
                                GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::Nil)),
                                GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::Nil)),
                            ]),
                        )
                    ])),
                    property_list: Some(PropertyList(vec![VerbList::new(
                        Verb::A,
                        ObjectList(vec![
                            GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::BooleanLiteral(false))),
                            GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::Nil)),
                        ]),
                    )]))
                }
            ))
        );
    }

    #[test]
    fn is_triples_node() {
        assert_eq!(
            triples_node("(true ())"),
            Ok((
                "",
                TriplesNode::Collection(Collection(vec![
                    GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::BooleanLiteral(true))),
                    GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::Nil))
                ]))
            ))
        );

        assert_eq!(
            triples_node("[\na false,() \n]"),
            Ok((
                "",
                TriplesNode::BlankNodePropertyList(PropertyList(vec![VerbList::new(
                    Verb::A,
                    ObjectList(vec![
                        GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::BooleanLiteral(false))),
                        GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::Nil)),
                    ]),
                )]))
            ))
        );
    }

    #[test]
    fn is_triples_template() {
        let nil = TriplesSameSubject::Node {
            triples_node: TriplesNode::Collection(Collection(vec![
                GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::BooleanLiteral(true))),
                GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::Nil)),
            ])),
            property_list: None,
        };

        assert_eq!(triples_template("(true())."), Ok(("", vec![nil.clone()])));

        assert_eq!(
            triples_template("(true()).(true())"),
            Ok(("", vec![nil.clone(), nil.clone()]))
        );

        assert_eq!(
            triples_template("(true()).[   a (),()\t] a false,()"),
            Ok((
                "",
                vec![
                    nil.clone(),
                    TriplesSameSubject::Node {
                        triples_node: TriplesNode::BlankNodePropertyList(PropertyList(vec![
                            VerbList::new(
                                Verb::A,
                                ObjectList(vec![
                                    GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::Nil)),
                                    GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::Nil)),
                                ]),
                            )
                        ])),
                        property_list: Some(PropertyList(vec![VerbList::new(
                            Verb::A,
                            ObjectList(vec![
                                GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::BooleanLiteral(
                                    false
                                ))),
                                GraphNode::VarOrTerm(VarOrTerm::Term(GraphTerm::Nil)),
                            ]),
                        )]))
                    }
                ]
            ))
        );
    }

}
