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
    type Variable: Pretty;

    fn is_known(&self) -> bool;

    fn typ(&self) -> Self;
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Evaluation {
    Static,
    Dynamic,
}

enum GenericTerm<'src, Term: TermReference<'src>> {
    Type,
    Apply {
        function: Term,
        argument: Term,
        typ: Term,
        evaluation: Evaluation,
    },
    Variable(Term::Variable),
    Unknown {
        typ: Term,
    },
    Let {
        value: Term,
        binding: VariableBinding<'src, Term>,
    },
    Pi(VariableBinding<'src, Term>),
    Lambda(VariableBinding<'src, Term>),
}

impl<'src, Term: TermReference<'src>> GenericTerm<'src, Term> {
    fn pi(name: Option<&'src str>, variable: Term, in_term: Term, evaluation: Evaluation) -> Self {
        Self::Pi(VariableBinding {
            name,
            variable,
            in_term,
            evaluation,
        })
    }

    fn is_known(&self) -> bool {
        !matches!(self, Self::Unknown { .. })
    }

    fn typ(
        &self,
        new: impl FnOnce(Self) -> Term,
        type_of_variable: impl FnOnce(&Term::Variable) -> Term,
    ) -> Term {
        match self {
            Self::Type => new(Self::Type),
            Self::Apply { typ, .. } | Self::Unknown { typ } => typ.clone(),
            Self::Variable(variable) => type_of_variable(variable),
            Self::Let { binding, .. } => binding.in_term.typ(),
            Self::Pi(_) => new(Self::Type),
            Self::Lambda(binding) => new(Self::pi(
                binding.name,
                binding.variable.clone(),
                binding.in_term.typ(),
                binding.evaluation,
            )),
        }
    }
}

struct VariableBinding<'src, Term: TermReference<'src>> {
    name: Option<&'src str>,
    variable: Term,
    in_term: Term,
    evaluation: Evaluation,
}

impl<'src, Term: TermReference<'src>> VariableBinding<'src, Term> {
    fn binding_type(&self) -> Term {
        self.variable.typ()
    }
}
