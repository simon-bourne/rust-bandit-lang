use std::{
    cell::{Ref, RefCell, RefMut},
    error::Error,
    fmt,
    rc::Rc,
    result,
};

pub mod ast;
mod constraints;
pub mod context;
pub mod core;
mod pretty;

pub use pretty::Pretty;

struct SharedMut<T>(Rc<RefCell<T>>);

impl<T> SharedMut<T> {
    pub fn new(x: T) -> Self {
        Self(Rc::new(RefCell::new(x)))
    }

    pub fn is_same(x: &Self, y: &Self) -> bool {
        Rc::ptr_eq(&x.0, &y.0)
    }

    pub fn borrow(&self) -> Ref<'_, T> {
        self.0.borrow()
    }

    pub fn borrow_mut(&self) -> RefMut<'_, T> {
        self.0.borrow_mut()
    }

    pub fn replace_with(&self, x: T) -> T {
        RefCell::replace(self.0.as_ref(), x)
    }

    pub fn id(&self) -> impl fmt::Debug {
        Rc::as_ptr(&self.0)
    }
}

impl<T> Clone for SharedMut<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

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

#[derive(Clone)]
struct VariableBinding<Term> {
    variable: Term,
    in_term: Term,
    evaluation: Evaluation,
}
