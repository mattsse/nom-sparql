use std::fmt;

use crate::{
    ask::AskQuery,
    clauses::SolutionModifier,
    construct::ConstructQuery,
    data::{DataBlock, DataSetClause},
    describe::DescribeQuery,
    expression::Iri,
    graph::GroupGraphPattern,
    select::SelectQuery,
};

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

#[derive(Debug, Clone, new, Eq, PartialEq)]
pub struct PrefixDecl {
    pub pname_ns: Option<String>,
    pub iri_ref: String,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Prologue(pub Vec<BaseOrPrefixDecl>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BaseOrPrefixDecl {
    Base(String),
    Prefix(PrefixDecl),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum VarOrIri {
    Var(Var),
    Iri(Iri),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Var {
    QMark(String),
    Dollar(String),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{base_decl, prefix_decl, prologue, var, var_or_iri};

    #[test]
    fn is_var_or_iri() {
        assert_eq!(
            var_or_iri("?name"),
            Ok(("", VarOrIri::Var(Var::QMark("name".to_string()))))
        );
    }

    #[test]
    fn is_var() {
        assert_eq!(var("?name"), Ok(("", Var::QMark("name".to_string()))));

        assert_eq!(var("$name"), Ok(("", Var::Dollar("name".to_string()))));
    }

    #[test]
    fn is_base() {
        assert_eq!(
            base_decl("Base <http://xmlns.com/foaf/0.1/>"),
            Ok(("", "http://xmlns.com/foaf/0.1/"))
        );
    }

    #[test]
    fn is_prologue() {
        assert_eq!(
            prologue(
                r#"PREFIX foaf:   <http://xmlns.com/foaf/0.1/>
            PREFIX :    <http://example.com/ns#>
            BASE <http://example.org/book/>"#
            ),
            Ok((
                "",
                Prologue(vec![
                    BaseOrPrefixDecl::Prefix(PrefixDecl::new(
                        Some("foaf".to_string()),
                        "http://xmlns.com/foaf/0.1/".to_string()
                    )),
                    BaseOrPrefixDecl::Prefix(PrefixDecl::new(
                        None,
                        "http://example.com/ns#".to_string()
                    )),
                    BaseOrPrefixDecl::Base("http://example.org/book/".to_string())
                ])
            ))
        );
    }

    #[test]
    fn is_prefix_decl() {
        assert_eq!(
            prefix_decl("PREFIX foaf: <http://xmlns.com/foaf/0.1/>"),
            Ok((
                "",
                PrefixDecl::new(
                    Some("foaf".to_string()),
                    "http://xmlns.com/foaf/0.1/".to_string()
                )
            ))
        );

        assert_eq!(
            prefix_decl("prefix   :   <http://xmlns.com/foaf/0.1/>"),
            Ok((
                "",
                PrefixDecl::new(None, "http://xmlns.com/foaf/0.1/".to_string())
            ))
        );
    }
}
