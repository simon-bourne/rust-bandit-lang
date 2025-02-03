use std::{cell::RefCell, fmt, rc::Rc, result};

pub mod context;
pub mod inference;
mod pretty;
pub mod type_annotated;

pub use pretty::Pretty;

type SharedMut<T> = Rc<RefCell<T>>;

pub type Result<T> = result::Result<T, InferenceError>;

#[derive(Debug)]
pub struct InferenceError;

pub trait ExpressionReference<'src>: Pretty + Clone + Sized {
    type Variable: Pretty;

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
    Let(VariableBinding<'src, Expr>),
    Pi(VariableBinding<'src, Expr>),
    Lambda(VariableBinding<'src, Expr>),
    Variable(Expr::Variable),
}

impl<'src, Expr: ExpressionReference<'src>> GenericExpression<'src, Expr> {
    fn typ(
        &self,
        new: impl FnOnce(Self) -> Expr,
        variable_type: impl FnOnce(&Expr::Variable) -> Expr,
    ) -> Expr {
        match self {
            GenericExpression::TypeOfType => new(GenericExpression::TypeOfType),
            GenericExpression::Constant { typ, .. } => typ.clone(),
            GenericExpression::Apply { typ, .. } => typ.clone(),
            GenericExpression::Let(variable_binding) => variable_binding.in_expression.typ(),
            GenericExpression::Pi(_) => new(GenericExpression::TypeOfType),
            GenericExpression::Lambda(variable_binding) => {
                new(GenericExpression::Pi(VariableBinding {
                    name: "_",
                    variable_value: variable_binding.variable_value.clone(),
                    in_expression: variable_binding.in_expression.typ(),
                }))
            }
            GenericExpression::Variable(variable) => variable_type(variable),
        }
    }
}

struct VariableBinding<'src, Expr: ExpressionReference<'src>> {
    name: &'src str,
    variable_value: Expr,
    in_expression: Expr,
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
