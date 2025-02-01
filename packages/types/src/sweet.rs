use std::{marker::PhantomData, rc::Rc};

use context::VariableLookup;

use super::{
    context::Context,
    pretty::{Annotation, Document, Operator, Side},
};
use crate::{
    inference::InferenceExpression, pretty::TypeAnnotated, Expression, ExpressionReference, Pretty,
    Result, Variable, VariableBinding,
};

mod context;

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

pub type SourceExpression<'src> = SweetExpression<'src, &'src str>;

pub type NamesResolvedExpression<'src> = SweetExpression<'src, Variable<'src>>;

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

    pub fn unknown(typ: Self) -> Self {
        Self(Rc::new(SrcExprVariants::Unknown { typ }), PhantomData)
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
}

impl<'src> NamesResolvedExpression<'src> {
    pub fn link(&self, ctx: &mut Context<'src>) -> Result<InferenceExpression<'src>> {
        Ok(match self.0.as_ref() {
            SrcExprVariants::Known { expression } => expression.link(ctx)?,
            SrcExprVariants::TypeAnnotation { expression, typ } => {
                let expression = expression.link(ctx)?;
                let mut typ = typ.link(ctx)?;
                InferenceExpression::unify(&mut expression.typ(), &mut typ)?;
                expression
            }
            SrcExprVariants::Unknown { typ } => InferenceExpression::unknown(typ.link(ctx)?),
        })
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

impl<'src> SourceExpression<'src> {
    pub fn variable(name: &'src str) -> Self {
        Self::known(Expression::Variable(name))
    }

    pub fn resolve_names(&self) -> Result<NamesResolvedExpression<'src>> {
        self.resolve_names_with_lookup(&mut VariableLookup::default())
    }

    pub fn link(&self, ctx: &mut Context<'src>) -> Result<InferenceExpression<'src>> {
        self.resolve_names()?.link(ctx)
    }

    fn resolve_names_with_lookup(
        &self,
        lookup: &mut VariableLookup<'src>,
    ) -> Result<NamesResolvedExpression<'src>> {
        Ok(match self.0.as_ref() {
            SrcExprVariants::Known { expression } => expression.resolve_names(lookup)?,
            SrcExprVariants::TypeAnnotation { expression, typ } => {
                let expression = expression.resolve_names_with_lookup(lookup)?;
                let typ = typ.resolve_names_with_lookup(lookup)?;
                NamesResolvedExpression::new(SrcExprVariants::TypeAnnotation { expression, typ })
            }
            SrcExprVariants::Unknown { typ } => {
                let typ = typ.resolve_names_with_lookup(lookup)?;
                NamesResolvedExpression::new(SrcExprVariants::Unknown { typ })
            }
        })
    }
}

impl<'src> Expression<'src, SourceExpression<'src>> {
    fn resolve_names(
        &self,
        lookup: &mut VariableLookup<'src>,
    ) -> Result<NamesResolvedExpression<'src>> {
        let expr = match self {
            Self::TypeOfType => Expression::TypeOfType,
            Self::Constant { name, typ } => Expression::Constant {
                name,
                typ: typ.resolve_names()?,
            },
            Self::Apply {
                function,
                argument,
                typ,
            } => Expression::Apply {
                function: function.resolve_names_with_lookup(lookup)?,
                argument: argument.resolve_names_with_lookup(lookup)?,
                typ: typ.resolve_names_with_lookup(lookup)?,
            },
            Self::Let(binding) => Expression::Let(binding.resolve_names(lookup)?),
            Self::FunctionType(binding) => Expression::FunctionType(binding.resolve_names(lookup)?),
            Self::Lambda(binding) => Expression::Lambda(binding.resolve_names(lookup)?),
            Self::Variable(name) => Expression::Variable(lookup.lookup(name)),
        };

        Ok(NamesResolvedExpression::new(SrcExprVariants::Known {
            expression: expr,
        }))
    }
}

impl<'src> VariableBinding<'src, SourceExpression<'src>> {
    fn resolve_names(
        &self,
        lookup: &mut VariableLookup<'src>,
    ) -> Result<VariableBinding<'src, NamesResolvedExpression<'src>>> {
        let variable_value = self.variable_value.resolve_names_with_lookup(lookup)?;

        lookup.with_variable(self.name, |lookup| {
            let in_expression = self.in_expression.resolve_names_with_lookup(lookup)?;
            Ok(VariableBinding {
                name: self.name,
                variable_value,
                in_expression,
            })
        })
    }
}

impl<'src> Expression<'src, NamesResolvedExpression<'src>> {
    fn link(&self, ctx: &mut Context<'src>) -> Result<InferenceExpression<'src>> {
        Ok(InferenceExpression::new(match self {
            Self::TypeOfType => Expression::TypeOfType,
            Self::Constant { name, typ } => Expression::Constant {
                name,
                typ: typ.link(ctx)?,
            },
            Self::Apply {
                function,
                argument,
                typ,
            } => Expression::Apply {
                function: function.link(ctx)?,
                argument: argument.link(ctx)?,
                typ: typ.link(ctx)?,
            },
            Self::Let(binding) => Expression::Let(binding.link(ctx)?),
            Self::FunctionType(binding) => Expression::FunctionType(binding.link(ctx)?),
            Self::Lambda(binding) => Expression::Lambda(binding.link(ctx)?),
            Self::Variable(variable) => return ctx.lookup_value(*variable),
        }))
    }
}

impl<'src> VariableBinding<'src, NamesResolvedExpression<'src>> {
    fn link(
        &self,
        ctx: &mut Context<'src>,
    ) -> Result<VariableBinding<'src, InferenceExpression<'src>>> {
        let name = self.name;
        let variable_value = self.variable_value.link(ctx)?;
        let in_expression = ctx.with_variable(name, variable_value.clone(), |ctx| {
            self.in_expression.link(ctx)
        })?;

        Ok(VariableBinding {
            name,
            variable_value,
            in_expression,
        })
    }
}
