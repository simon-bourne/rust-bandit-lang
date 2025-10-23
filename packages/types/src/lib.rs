use std::{error::Error, fmt, result};

use derive_more::Constructor;
pub use pretty::Pretty;

pub mod ast;
pub mod core;
mod pretty;

pub struct DeBruijnIndex(usize);

pub type Result<T> = result::Result<T, InferenceError>;

// TODO: Better error handling.
#[derive(Debug)]
pub struct InferenceError;

impl fmt::Display for InferenceError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("InferenceError")
    }
}

impl Error for InferenceError {}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Evaluation {
    Static,
    Dynamic,
}

#[derive(Constructor)]
pub struct VariableBinding<'src, Term> {
    variable: Variable<'src, Term>,
    in_term: Term,
    evaluation: Evaluation,
}

#[derive(Constructor)]
pub struct Variable<'src, Term> {
    name: Option<&'src str>,
    typ: Option<Term>,
}
