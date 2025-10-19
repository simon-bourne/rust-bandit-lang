use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
    result,
};

mod constraints;
pub mod context;
pub mod linked;
mod pretty;
pub mod source;

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
}

impl<T> Clone for SharedMut<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

pub type Result<T> = result::Result<T, InferenceError>;

#[derive(Debug)]
pub struct InferenceError;

pub trait TermReference<'src>: Pretty + Clone + Sized {
    fn is_known(&self) -> bool;

    fn typ(&self) -> Self;
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Evaluation {
    Static,
    Dynamic,
}

struct VariableBinding<Term> {
    variable: Term,
    in_term: Term,
    evaluation: Evaluation,
}
