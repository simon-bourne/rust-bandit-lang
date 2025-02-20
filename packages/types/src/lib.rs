use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
    result,
};

pub mod context;
pub mod inference;
mod pretty;
pub mod source;
pub mod well_typed;

pub use pretty::Pretty;
use pretty::{Document, Layout, Operator, Side, TypeAnnotated};

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

    pub fn try_borrow_mut(&self) -> Result<RefMut<'_, T>> {
        self.0.try_borrow_mut().map_err(|_| InferenceError)
    }

    pub fn as_ptr(&self) -> *mut T {
        self.0.as_ptr()
    }

    pub fn replace_with(&self, x: T) {
        RefCell::replace(self.0.as_ref(), x);
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
    type VariableValue: TermReference<'src>;

    fn is_known(&self) -> bool;

    fn typ(&self) -> Self;
}

enum GenericTerm<'src, Term: TermReference<'src>> {
    TypeOfType,
    Constant {
        name: &'src str,
        typ: Term,
    },
    Apply {
        function: Term,
        argument: Term,
        typ: Term,
    },
    Variable(Term::Variable),
    VariableBinding(VariableBinding<'src, Term>),
}

impl<'src, Term: TermReference<'src>> GenericTerm<'src, Term> {
    fn pi(name: Option<&'src str>, variable_value: Term::VariableValue, in_term: Term) -> Self {
        Self::VariableBinding(VariableBinding {
            name,
            binder: Binder::Pi,
            variable_value,
            in_term,
        })
    }

    fn typ(
        &self,
        new: impl FnOnce(Self) -> Term,
        variable_type: impl FnOnce(&Term::Variable) -> Term,
    ) -> Term {
        match self {
            GenericTerm::TypeOfType => new(GenericTerm::TypeOfType),
            GenericTerm::Constant { typ, .. } => typ.clone(),
            GenericTerm::Apply { typ, .. } => typ.clone(),
            GenericTerm::Variable(variable) => variable_type(variable),
            GenericTerm::VariableBinding(binding) => match binding.binder {
                Binder::Let => binding.in_term.typ(),
                Binder::Pi => new(GenericTerm::TypeOfType),
                Binder::Lambda => new(GenericTerm::pi(
                    None,
                    binding.variable_value.clone(),
                    binding.in_term.typ(),
                )),
            },
        }
    }
}

#[derive(Copy, Clone)]
enum Binder {
    /// `Let` is included so we can generalize explicitly using type
    /// annotations. Otherwise, we could replace `let x = y in z` with `(\x =>
    /// z) y`.
    Let,
    Pi,
    Lambda,
}

struct VariableBinding<'src, Term: TermReference<'src>> {
    name: Option<&'src str>,
    binder: Binder,
    variable_value: Term::VariableValue,
    in_term: Term,
}

#[derive(Clone)]
pub enum VariableValue<Term> {
    Known { value: Term },
    Unknown { typ: Term },
}

impl<'src, Term: TermReference<'src>> TermReference<'src> for VariableValue<Term> {
    type Variable = Term::Variable;
    type VariableValue = Term::VariableValue;

    fn is_known(&self) -> bool {
        match self {
            Self::Known { value } => value.is_known(),
            Self::Unknown { .. } => false,
        }
    }

    fn typ(&self) -> Self {
        match self {
            Self::Known { value } => Self::Known { value: value.typ() },
            Self::Unknown { typ } => Self::Known { value: typ.clone() },
        }
    }
}

impl<'src, Term: TermReference<'src>> Pretty for VariableValue<Term> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        match self {
            Self::Known { value } => value.to_document(parent, layout),
            Self::Unknown { typ } => TypeAnnotated::new(None, typ).to_document(parent, layout),
        }
    }
}
