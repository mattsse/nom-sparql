use crate::query::Var;

use nom::IResult;

#[derive(Debug, Clone)]
pub struct Expression(ConditionalOrExpression);

#[derive(Debug, Clone)]
pub enum NumericExpression {
    EQ(AdditiveExpression),
    NE(AdditiveExpression),
    LT(AdditiveExpression),
    GT(AdditiveExpression),
    LE(AdditiveExpression),
    GE(AdditiveExpression),
    Default(AdditiveExpression),
}

#[derive(Debug, Clone)]
pub struct RelationalExpression {
    pub lhs: NumericExpression,
    pub rhs: Option<NumericExpression>,
}

#[derive(Debug, Clone)]
pub struct ConditionalAndExpression {
    pub lhs: RelationalExpression,
    pub rhs: Vec<RelationalExpression>,
}

#[derive(Debug, Clone)]
pub struct ConditionalOrExpression {
    pub lhs: ConditionalAndExpression,
    pub rhs: Vec<ConditionalAndExpression>,
}

#[derive(Debug, Clone)]
pub struct AdditiveExpression {
    pub lhs: MultiplicativeExpression,
    pub rhs: Vec<AddExpression>,
}

#[derive(Debug, Clone)]
pub enum AddExpression {
    Add(MultiplicativeExpression),
    Sub(MultiplicativeExpression),
    NegNumericLiteral(NumLiteral),
    PosNumericLiteral(NumLiteral),
}

#[derive(Debug, Clone)]
pub enum NumLiteral {
    Int(usize),
    Decimal(f64),
    Double(f64),
}

#[derive(Debug, Clone)]
pub struct MultiplicativeExpression {
    pub lhs: UnaryExpression,
    pub rhs: Vec<MultExpression>,
}

#[derive(Debug, Clone)]
pub enum MultExpression {
    Mult(UnaryExpression),
    Div(UnaryExpression),
}

#[derive(Debug, Clone)]
pub enum UnaryExpression {
    Not(PrimaryExpression),
    Add(PrimaryExpression),
    Sub(PrimaryExpression),
    Default(PrimaryExpression),
}

#[derive(Debug, Clone)]
pub enum PrimaryExpression {
    BrackettedExpression(Box<Expression>),
    BuiltInCall(BuiltInCall),
    IriRefOrFunction,
    RdfLiteral,
    NumericLiteral,
    BooleanLiteral,
    Var,
}

#[derive(Debug, Clone)]
pub enum NumericLiteral {
    Unsigned(NumLiteral),
    Pos(NumLiteral),
    Neg(NumLiteral),
}

#[derive(Debug, Clone)]
pub struct BooleanLiteral(bool);

#[derive(Debug, Clone)]
pub enum BuiltInCall {
    Str(Box<Expression>),
    Lang(Box<Expression>),
    LangMatches {
        first: Box<Expression>,
        second: Box<Expression>,
    },
    Datatype(Box<Expression>),
    Bound(Var),
    SameTerm {
        first: Box<Expression>,
        second: Box<Expression>,
    },
    IsIri(Box<Expression>),
    IsUri(Box<Expression>),
    IsBlank(Box<Expression>),
    IsLiteral(Box<Expression>),
    Regex(Box<RegexExpression>),
}

#[derive(Debug, Clone)]
pub struct RegexExpression {}

#[derive(Debug, Clone)]
pub enum IriRef {
    IriRef(String),
    PrefixedName(String),
}

#[derive(Debug, Clone)]
pub enum PrefixedName {
    PnameLN {
        pn_prefix: Option<String>,
        pn_local: String,
    },
    PnameNS {
        pn_prefix: Option<String>,
    },
}

#[derive(Debug, Clone)]
pub enum Order {
    Asc,
    Desc,
}

#[derive(Debug, Clone)]
pub struct OrderExpression {
    pub order: Order,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub enum ArgList {
    Nil,
    Expression {
        first: Box<Expression>,
        further: Vec<Expression>,
    },
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub iri_ref: IriRef,
    pub args: ArgList,
}

#[derive(Debug, Clone)]
pub enum Constraint {
    Bracketted(Box<Expression>),
    BuiltInCall(BuiltInCall),
    FunctionCall(Box<FunctionCall>),
}
