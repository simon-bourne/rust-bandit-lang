use std::{cell::RefCell, fmt, rc::Rc, result};

pub mod context;
pub mod inference;
mod pretty;
pub mod type_annotated;
pub mod well_typed;

pub use pretty::Pretty;

type SharedMut<T> = Rc<RefCell<T>>;

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
    /// `Let` is included so we can generalize explicitly using type
    /// annotations. Otherwise, we could replace `let x = y in z` with `(\x =>
    /// z) y`.
    Let(VariableBinding<'src, Expr>),
    Pi(VariableBinding<'src, Expr>),
    Lambda(VariableBinding<'src, Expr>),
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
            Self::Let(variable_binding) => Self::Let(variable_binding.map_expression(f)),
            Self::Pi(variable_binding) => Self::Pi(variable_binding.map_expression(f)),
            Self::Lambda(variable_binding) => Self::Lambda(variable_binding.map_expression(f)),
        }
    }

    fn typ(&self, new: impl FnOnce(Self) -> Expr) -> Expr {
        match self {
            GenericExpression::TypeOfType => new(GenericExpression::TypeOfType),
            GenericExpression::Constant { typ, .. } => typ.clone(),
            GenericExpression::Apply { typ, .. } => typ.clone(),
            GenericExpression::Let(variable_binding) => variable_binding.in_expression.typ(),
            GenericExpression::Pi(_) => new(GenericExpression::TypeOfType),
            GenericExpression::Lambda(variable_binding) => {
                new(GenericExpression::Pi(VariableBinding {
                    name: None,
                    variable_value: variable_binding.variable_value.clone(),
                    in_expression: variable_binding.in_expression.typ(),
                }))
            }
        }
    }
}

struct VariableBinding<'src, Expr> {
    name: Option<&'src str>,
    variable_value: Expr,
    in_expression: Expr,
}

impl<Expr> VariableBinding<'_, Expr> {
    fn map_expression(&self, mut f: impl FnMut(&Expr) -> Expr) -> Self {
        Self {
            name: self.name,
            variable_value: f(&self.variable_value),
            in_expression: f(&self.in_expression),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
struct DeBruijnIndex(usize);

#[derive(Copy, Clone, Eq, PartialEq)]
enum VariableScope {
    Local(DeBruijnIndex),
    Global,
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Variable<'src> {
    name: &'src str,
    scope: VariableScope,
}

impl fmt::Debug for Variable<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.to_pretty_string(80))
    }
}

pub struct VariableReference<'src, Value> {
    name: &'src str,
    value: Value,
}

impl<'src, Value: ExpressionReference<'src>> fmt::Debug for VariableReference<'src, Value> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.to_pretty_string(80))
    }
}
