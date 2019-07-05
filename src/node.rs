use crate::expression::{Constraint, IriRef};
use crate::literal::NumericLiteral;
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
    Term(GraphTerm),
}

#[derive(Debug, Clone)]
pub struct ObjectList(pub Vec<GraphNode>);

#[derive(Debug, Clone)]
pub struct VerbList {
    pub verb: Verb,
    pub object_list: ObjectList,
}

impl VerbList {
    pub fn new(verb: Verb, object_list: ObjectList) -> Self {
        VerbList { verb, object_list }
    }
}

#[derive(Debug, Clone)]
pub struct PropertyList(pub Vec<VerbList>);

#[derive(Debug, Clone)]
pub struct Collection(pub Vec<GraphNode>);

#[derive(Debug, Clone)]
pub enum GraphTerm {
    IriRef(IriRef),
    RdfLiteral(RdfLiteral),
    NumericLiteral(NumericLiteral),
    BooleanLiteral(bool),
    BlankNode(BlankNode),
    /// empty parentheses
    Nil,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RdfLiteral {
    pub literal: String,
    pub descriptor: Option<RdfLiteralDescriptor>,
}

impl RdfLiteral {
    /// creates a complete [`RdfLiteral`]
    pub fn new<T: ToString>(literal: T, descriptor: RdfLiteralDescriptor) -> Self {
        RdfLiteral {
            literal: literal.to_string(),
            descriptor: Some(descriptor),
        }
    }

    /// creates a new [`RdfLiteral`] with a `literal` only
    pub fn literal<T: ToString>(literal: T) -> Self {
        RdfLiteral {
            literal: literal.to_string(),
            descriptor: None,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum RdfLiteralDescriptor {
    LangTag(String),
    IriRef(IriRef),
}

#[derive(Debug, Clone)]
pub enum BlankNode {
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
        triples_node: TriplesNode,
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
