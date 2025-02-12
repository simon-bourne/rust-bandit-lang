use std::{marker::PhantomData, rc::Rc};

use super::pretty::{Document, Layout, Operator, Side};
use crate::{
    pretty::TypeAnnotated, Binder, ExpressionReference, GenericExpression, Pretty, VariableBinding,
};

pub mod named_locals;

#[derive(Clone)]
pub struct Expression<'src, Var: Pretty + Clone>(
    Rc<ExprVariants<'src, Self, Var>>,
    PhantomData<Var>,
);

impl<'src, Var: Pretty + Clone> ExpressionReference<'src> for Expression<'src, Var> {
    fn is_known(&self) -> bool {
        self.0.is_known()
    }

    fn typ(&self) -> Self {
        self.0.typ(Self::known, |_| Self::unknown_type())
    }
}

impl<Var: Pretty + Clone> Pretty for Expression<'_, Var> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        match self.0.as_ref() {
            ExprVariants::Known { expression } => expression.to_document(parent, layout),
            ExprVariants::Variable(variable) => variable.to_document(parent, layout),
            ExprVariants::TypeAnnotation { expression, typ } => {
                TypeAnnotated::new(expression, typ).to_document(parent, layout)
            }
            ExprVariants::Unknown { typ } => {
                TypeAnnotated::new(None, typ).to_document(parent, layout)
            }
        }
    }
}

impl<'src, Var: Pretty + Clone> Expression<'src, Var> {
    pub fn type_of_type() -> Self {
        Self::known(GenericExpression::TypeOfType)
    }

    pub fn unknown_value() -> Self {
        Self::new(ExprVariants::Unknown {
            typ: Self::unknown_type(),
        })
    }

    pub fn unknown_type() -> Self {
        Self::new(ExprVariants::Unknown {
            typ: Self::type_of_type(),
        })
    }

    pub fn type_constant(name: &'src str) -> Self {
        Self::known(GenericExpression::Constant {
            name,
            typ: Self::type_of_type(),
        })
    }

    pub fn constant(name: &'src str, typ: Self) -> Self {
        Self::known(GenericExpression::Constant { name, typ })
    }

    pub fn apply(self, argument: Self) -> Self {
        Self::known(GenericExpression::Apply {
            function: self,
            argument,
            typ: Self::unknown_type(),
        })
    }

    pub fn let_binding(name: &'src str, variable_value: Self, in_expression: Self) -> Self {
        Self::binding(Binder::Let, Some(name), variable_value, in_expression)
    }

    pub fn pi_type(name: Option<&'src str>, variable_value: Self, result_type: Self) -> Self {
        Self::binding(Binder::Pi, name, variable_value, result_type)
    }

    pub fn lambda(name: &'src str, variable_value: Self, in_expression: Self) -> Self {
        Self::binding(Binder::Lambda, Some(name), variable_value, in_expression)
    }

    pub fn has_type(self, typ: Self) -> Self {
        Self::new(ExprVariants::TypeAnnotation {
            expression: self,
            typ,
        })
    }

    fn new(expr: ExprVariants<'src, Self, Var>) -> Self {
        Self(Rc::new(expr), PhantomData)
    }

    fn known(expression: GenericExpression<'src, Self>) -> Self {
        Self::new(ExprVariants::Known { expression })
    }

    fn binding(
        binder: Binder,
        name: Option<&'src str>,
        variable_value: Self,
        in_expression: Self,
    ) -> Self {
        Self::known(GenericExpression::VariableBinding(VariableBinding {
            name,
            binder,
            variable_value,
            in_expression,
        }))
    }
}

enum ExprVariants<'src, Expr: ExpressionReference<'src>, Var> {
    Known {
        expression: GenericExpression<'src, Expr>,
    },
    TypeAnnotation {
        expression: Expr,
        typ: Expr,
    },
    Unknown {
        typ: Expr,
    },
    Variable(Var),
}

impl<'src, Expr: ExpressionReference<'src>, Var> ExprVariants<'src, Expr, Var> {
    fn is_known(&self) -> bool {
        match self {
            Self::Known { .. } | Self::Variable(_) => true,
            Self::TypeAnnotation { expression, .. } => expression.is_known(),
            Self::Unknown { .. } => false,
        }
    }

    fn typ(
        &self,
        new: impl FnOnce(GenericExpression<'src, Expr>) -> Expr,
        variable_type: impl FnOnce(&Var) -> Expr,
    ) -> Expr {
        match self {
            Self::Known { expression } => expression.typ(new),
            Self::Variable(variable) => variable_type(variable),
            Self::TypeAnnotation { typ, .. } | Self::Unknown { typ, .. } => typ.clone(),
        }
    }
}
