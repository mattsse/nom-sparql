use crate::expression::Constraint;
use crate::query::{Var, VarOrIriRef};
use crate::triple::Verb;

#[derive(Debug, Clone)]
pub enum GraphNode {
    VarOrTerm(VarOrTerm),
    TriplesNode(Box<TriplesNode>),
}
#[derive(Debug, Clone)]
pub enum VarOrTerm {
    Var(Var),
}

#[derive(Debug, Clone)]
pub struct ObjectList {
    pub first: GraphNode,
    pub further: Vec<GraphNode>,
}

#[derive(Debug, Clone)]
pub struct VerbList {
    pub verb: Verb,
    pub object_list: ObjectList,
}

#[derive(Debug, Clone)]
pub struct PropertyList {
    pub first: VerbList,
    pub further: Vec<Option<VerbList>>,
}

#[derive(Debug, Clone)]
pub struct Collection(Vec<GraphNode>);

#[derive(Debug, Clone)]
pub enum GraphTerm {
    IriRef(VarOrIriRef),
    RdfLiteral,
    NumericLiteral,
    BooleanLiteral(bool),
    BlankNode(BlankNode),
    /// empty parentheses
    Nil,
}

#[derive(Debug, Clone)]
pub enum BlankNode {
    /// empty brackets
    Anon,
    Label(String),
}

#[derive(Debug, Clone)]
pub enum TriplesNode {
    Collection(Collection),
    BlankNodePropertyList(PropertyList),
}

#[derive(Debug, Clone)]
pub enum TriplesSameSubject {
    Term {
        var_or_term: VarOrTerm,
        property_list: PropertyList,
    },
    Node {
        triples: TriplesNode,
        property_list: Option<PropertyList>,
    },
}

#[derive(Debug, Clone)]
pub struct ConstructTriples {
    pub first_triples: TriplesSameSubject,
    pub further_triples: Vec<TriplesSameSubject>,
}

#[derive(Debug, Clone)]
pub struct TriplesBlock {
    pub first_triples: TriplesSameSubject,
    pub further_triples: Vec<TriplesSameSubject>,
}

#[derive(Debug, Clone)]
pub struct GroupGraphPattern {
    pub triples_block: Option<TriplesBlock>,
    pub pattern: Vec<GraphPattern>,
}

#[derive(Debug, Clone)]
pub struct GraphPattern {
    pub pattern_or_filter: GraphPatternOrFilter,
    pub triples_block: Option<TriplesBlock>,
}

#[derive(Debug, Clone)]
pub enum GraphPatternOrFilter {
    GraphPattern(Box<GraphPatternNotTriples>),
    Filter(Constraint),
}

#[derive(Debug, Clone)]
pub enum GraphPatternNotTriples {
    Optional(GroupGraphPattern),
    GroupOrUnion(GroupOrUnionGraphPattern),
    Graph(GraphGraphPattern),
}

#[derive(Debug, Clone)]
pub struct GroupOrUnionGraphPattern {
    pub first: GroupGraphPattern,
    pub unions: Vec<GroupGraphPattern>,
}

#[derive(Debug, Clone)]
pub struct GraphGraphPattern {
    pub var_or_iri_ref: VarOrIriRef,
    pub pattern: GroupGraphPattern,
}
