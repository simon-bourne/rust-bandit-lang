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
    Variable(Term::Variable),
    VariableBinding(VariableBinding<'src, Term>),
}

impl<'src, Term: TermReference<'src, Type = Term>> GenericTerm<'src, Term> {
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
pub struct Variable<'src, Term> {
    name: Option<&'src str>,
    value: SharedMut<VariableValue<Term>>,
}

impl<'src, Term> Variable<'src, Term> {
    pub fn known(name: Option<&'src str>, value: Term) -> Self {
        Self {
            name,
            value: SharedMut::new(VariableValue::Known { value }),
        }
    }

    pub fn unknown(name: Option<&'src str>, typ: Term) -> Self {
        Self {
            name,
            value: SharedMut::new(VariableValue::Unknown { typ }),
        }
    }
}

#[derive(Clone)]
pub enum VariableValue<Term> {
    Known { value: Term },
    Unknown { typ: Term },
}

impl<'src, Term: TermReference<'src, Type = Term>> TermReference<'src> for Variable<'src, Term> {
    type Type = Term::Type;
    type Variable = Term::Variable;
    type VariableValue = Term::VariableValue;

    fn is_known(&self) -> bool {
        match &*self.value.borrow() {
            VariableValue::Known { value } => value.is_known(),
            VariableValue::Unknown { .. } => false,
        }
    }

    fn typ(&self) -> Self::Type {
        match &*self.value.borrow() {
            VariableValue::Known { value } => value.typ(),
            VariableValue::Unknown { typ } => typ.clone(),
        }
    }
}

// TODO: Use `VariableValue` for all term types (remove well_typed),then remove
// this impl
impl<'src, Term: TermReference<'src, Type = Term>> TermReference<'src> for VariableValue<Term> {
    type Type = Term::Type;
    type Variable = Term::Variable;
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
