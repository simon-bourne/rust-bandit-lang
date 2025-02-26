use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
    result,
};

pub mod context;
mod de_bruijn;
pub mod linked;
mod pretty;
pub mod source;
pub mod well_typed;

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
    type VariableId: AsRef<str>;
    type VariableReference: Pretty;
    type VariableValue: TermReference<'src>;
    type Type: TermReference<'src>;

    fn is_known(&self) -> bool;

    fn typ(&self) -> Self::Type;
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
    Variable(Term::VariableReference),
    VariableBinding(VariableBinding<'src, Term>),
}

impl<'src, Term: TermReference<'src, Type = Term>> GenericTerm<'src, Term> {
    fn pi(
        name: Option<Term::VariableId>,
        variable_value: Term::VariableValue,
        in_term: Term,
    ) -> Self {
        Self::VariableBinding(VariableBinding {
            id: name,
            binder: Binder::Pi,
            variable_value,
            in_term,
        })
    }

    fn typ(
        &self,
        new: impl FnOnce(Self) -> Term,
        variable_type: impl FnOnce(&Term::VariableReference) -> Term,
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
    Let,
    Pi,
    Lambda,
}

struct VariableBinding<'src, Term: TermReference<'src>> {
    id: Option<Term::VariableId>,
    binder: Binder,
    variable_value: Term::VariableValue,
    in_term: Term,
}

#[derive(Clone)]
pub enum VariableValue<Term> {
    Known { value: Term },
    Unknown { typ: Term },
}

impl<'src, Term: TermReference<'src, Type = Term>> TermReference<'src> for VariableValue<Term> {
    type Type = Term::Type;
    type VariableId = Term::VariableId;
    type VariableReference = Term::VariableReference;
    type VariableValue = Term::VariableValue;

    fn is_known(&self) -> bool {
        match self {
            Self::Known { value } => value.is_known(),
            Self::Unknown { .. } => false,
        }
    }

    fn typ(&self) -> Self::Type {
        match self {
            Self::Known { value } => value.typ(),
            Self::Unknown { typ } => typ.clone(),
        }
    }
}
