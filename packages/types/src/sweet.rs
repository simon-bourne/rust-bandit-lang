use std::{marker::PhantomData, rc::Rc};

use super::pretty::{Annotation, Document, Operator, Side};
use crate::{pretty::TypeAnnotated, Expression, ExpressionReference, Pretty, VariableBinding};

mod names_resolved;
mod source;

pub use names_resolved::NamesResolvedExpression;
pub use source::SourceExpression;

#[derive(Clone)]
pub struct SweetExpression<'src, Var: Pretty + Clone>(
    Rc<SrcExprVariants<'src, Self>>,
    PhantomData<Var>,
);

impl<'src, Var: Pretty + Clone> ExpressionReference<'src> for SweetExpression<'src, Var> {
    type Variable = Var;

    fn is_known(&self) -> bool {
        self.0.is_known()
    }

    fn typ(&self) -> Self {
        self.0.typ(Self::known, |_| Self::unknown_type())
    }
}

impl<Var: Pretty + Clone> Pretty for SweetExpression<'_, Var> {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotation: Annotation) -> Document {
        match self.0.as_ref() {
            SrcExprVariants::Known { expression } => expression.to_document(parent, annotation),
            SrcExprVariants::TypeAnnotation { expression, typ } => {
                TypeAnnotated::new(expression, typ).to_document(parent, annotation)
            }
            SrcExprVariants::Unknown { typ } => {
                TypeAnnotated::new("_", typ).to_document(parent, annotation)
            }
        }
    }
}

impl<'src, Var: Pretty + Clone> SweetExpression<'src, Var> {
    pub fn type_of_type() -> Self {
        Self::known(Expression::TypeOfType)
    }

    pub fn unknown_type() -> Self {
        Self::unknown(Self::type_of_type())
    }

    pub fn unknown_term() -> Self {
        Self::unknown(Self::unknown_type())
    }

    pub fn type_constant(name: &'src str) -> Self {
        Self::known(Expression::Constant {
            name,
            typ: Self::type_of_type(),
        })
    }

    pub fn constant(name: &'src str, typ: Self) -> Self {
        Self::known(Expression::Constant { name, typ })
    }

    pub fn apply(self, argument: Self) -> Self {
        Self::known(Expression::Apply {
            function: self,
            argument,
            typ: Self::unknown_type(),
        })
    }

    pub fn let_binding(name: &'src str, variable_value: Self, in_expression: Self) -> Self {
        Self::known(Expression::Let(VariableBinding {
            name,
            variable_value,
            in_expression,
        }))
    }

    pub fn function_type(name: &'src str, argument_type: Self, result_type: Self) -> Self {
        Self::known(Expression::FunctionType(VariableBinding {
            name,
            variable_value: Self::unknown(argument_type),
            in_expression: result_type,
        }))
    }

    pub fn lambda(name: &'src str, argument_type: Self, in_expression: Self) -> Self {
        Self::known(Expression::Lambda(VariableBinding {
            name,
            variable_value: Self::unknown(argument_type),
            in_expression,
        }))
    }

    pub fn has_type(self, typ: Self) -> Self {
        Self::new(SrcExprVariants::TypeAnnotation {
            expression: self,
            typ,
        })
    }

    fn new(expr: SrcExprVariants<'src, Self>) -> Self {
        Self(Rc::new(expr), PhantomData)
    }

    fn known(expression: Expression<'src, Self>) -> Self {
        Self(Rc::new(SrcExprVariants::Known { expression }), PhantomData)
    }

    fn unknown(typ: Self) -> Self {
        Self(Rc::new(SrcExprVariants::Unknown { typ }), PhantomData)
    }
}

enum SrcExprVariants<'src, Expr: ExpressionReference<'src>> {
    Known { expression: Expression<'src, Expr> },
    TypeAnnotation { expression: Expr, typ: Expr },
    Unknown { typ: Expr },
}

impl<'src, Expr: ExpressionReference<'src>> SrcExprVariants<'src, Expr> {
    fn is_known(&self) -> bool {
        match self {
            Self::Known { .. } => true,
            Self::TypeAnnotation { expression, .. } => expression.is_known(),
            Self::Unknown { .. } => false,
        }
    }

    fn typ(
        &self,
        new: impl FnOnce(Expression<'src, Expr>) -> Expr,
        variable_type: impl FnOnce(&Expr::Variable) -> Expr,
    ) -> Expr {
        match self {
            Self::Known { expression } => expression.typ(new, variable_type),
            Self::TypeAnnotation { typ, .. } => typ.clone(),
            Self::Unknown { typ } => typ.clone(),
        }
    }
}
