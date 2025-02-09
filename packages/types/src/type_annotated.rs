use std::{marker::PhantomData, rc::Rc};

use super::pretty::{Annotation, Document, Operator, Side};
use crate::{
    inference, pretty::TypeAnnotated, ExpressionReference, GenericExpression, Pretty,
    VariableBinding,
};

pub mod indexed_locals;
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
        self.0.typ(Self::known, |_| Self::inferred_type())
    }
}

impl<Var: Pretty + Clone> Pretty for Expression<'_, Var> {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotation: Annotation) -> Document {
        match self.0.as_ref() {
            ExprVariants::Known { expression } => expression.to_document(parent, annotation),
            ExprVariants::Variable(variable) => variable.to_document(parent, annotation),
            ExprVariants::TypeAnnotation { expression, typ } => {
                TypeAnnotated::new(expression, typ).to_document(parent, annotation)
            }
            ExprVariants::LinkedUnknown { typ, .. } | ExprVariants::FreshUnknown { typ } => {
                TypeAnnotated::new("_", typ).to_document(parent, annotation)
            }
        }
    }
}

impl<'src, Var: Pretty + Clone> Expression<'src, Var> {
    pub fn type_of_type() -> Self {
        Self::known(GenericExpression::TypeOfType)
    }

    pub fn unknown_value() -> Self {
        Self::new(ExprVariants::LinkedUnknown {
            expression: inference::Expression::unknown_value(),
            typ: Self::unknown_type(),
        })
    }

    pub fn unknown_type() -> Self {
        Self::new(ExprVariants::LinkedUnknown {
            expression: inference::Expression::unknown_type(),
            typ: Self::type_of_type(),
        })
    }

    pub fn inferred_value() -> Self {
        Self::new(ExprVariants::FreshUnknown {
            typ: Self::inferred_type(),
        })
    }

    pub fn inferred_type() -> Self {
        Self::new(ExprVariants::FreshUnknown {
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
            typ: Self::inferred_type(),
        })
    }

    pub fn let_binding(name: &'src str, variable_value: Self, in_expression: Self) -> Self {
        Self::known(GenericExpression::Let(VariableBinding {
            name,
            variable_value,
            in_expression,
        }))
    }

    pub fn function_type(name: &'src str, variable_value: Self, result_type: Self) -> Self {
        Self::known(GenericExpression::Pi(VariableBinding {
            name,
            variable_value,
            in_expression: result_type,
        }))
    }

    pub fn lambda(name: &'src str, variable_value: Self, in_expression: Self) -> Self {
        Self::known(GenericExpression::Lambda(VariableBinding {
            name,
            variable_value,
            in_expression,
        }))
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
}

enum ExprVariants<'src, Expr: ExpressionReference<'src>, Var> {
    Known {
        expression: GenericExpression<'src, Expr>,
    },
    TypeAnnotation {
        expression: Expr,
        typ: Expr,
    },
    LinkedUnknown {
        expression: inference::Expression<'src>,
        typ: Expr,
    },
    FreshUnknown {
        typ: Expr,
    },
    Variable(Var),
}

impl<'src, Expr: ExpressionReference<'src>, Var> ExprVariants<'src, Expr, Var> {
    fn is_known(&self) -> bool {
        match self {
            Self::Known { .. } | Self::Variable(_) => true,
            Self::TypeAnnotation { expression, .. } => expression.is_known(),
            Self::LinkedUnknown { .. } | Self::FreshUnknown { .. } => false,
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
            Self::TypeAnnotation { typ, .. }
            | Self::LinkedUnknown { typ, .. }
            | Self::FreshUnknown { typ, .. } => typ.clone(),
        }
    }
}
