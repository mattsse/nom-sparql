use crate::query::VarOrIriRef;

#[derive(Clone, Debug)]
pub enum Verb {
    VarOrIriRef(VarOrIriRef),
    A,
}
