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

pub trait ExpressionReference<'src>: Pretty + Clone + Sized {
    fn is_known(&self) -> bool;

    fn typ(&self) -> Self;
}

enum GenericExpression<'src, Expr: ExpressionReference<'src>> {
    TypeOfType,
    Constant {
        name: &'src str,
        typ: Expr,
    },
    Apply {
        function: Expr,
        argument: Expr,
        typ: Expr,
    },
    VariableBinding(VariableBinding<'src, Expr>),
}

impl<'src, Expr: ExpressionReference<'src>> GenericExpression<'src, Expr> {
    fn map_expression(&self, mut f: impl FnMut(&Expr) -> Expr) -> Self {
        match self {
            Self::TypeOfType => Self::TypeOfType,
            Self::Constant { name, typ } => Self::Constant { name, typ: f(typ) },
            Self::Apply {
                function,
                argument,
                typ,
            } => Self::Apply {
                function: f(function),
                argument: f(argument),
                typ: f(typ),
            },
            Self::VariableBinding(variable_binding) => {
                Self::VariableBinding(variable_binding.map_expression(f))
            }
        }
    }

    fn pi(name: Option<&'src str>, variable_value: Expr, in_expression: Expr) -> Self {
        Self::VariableBinding(VariableBinding {
            name,
            binder: Binder::Pi,
            variable_value,
            in_expression,
        })
    }

    fn typ(&self, new: impl FnOnce(Self) -> Expr) -> Expr {
        match self {
            GenericExpression::TypeOfType => new(GenericExpression::TypeOfType),
            GenericExpression::Constant { typ, .. } => typ.clone(),
            GenericExpression::Apply { typ, .. } => typ.clone(),
            GenericExpression::VariableBinding(binding) => match binding.binder {
                Binder::Let => binding.in_expression.typ(),
                Binder::Pi => new(GenericExpression::TypeOfType),
                Binder::Lambda => new(GenericExpression::pi(
                    None,
                    binding.variable_value.clone(),
                    binding.in_expression.typ(),
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

struct VariableBinding<'src, Expr> {
    name: Option<&'src str>,
    binder: Binder,
    variable_value: Expr,
    in_expression: Expr,
}

impl<Expr> VariableBinding<'_, Expr> {
    fn map_expression(&self, mut f: impl FnMut(&Expr) -> Expr) -> Self {
        Self {
            name: self.name,
            binder: self.binder,
            variable_value: f(&self.variable_value),
            in_expression: f(&self.in_expression),
        }
    }
}
