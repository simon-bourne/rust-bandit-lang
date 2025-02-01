use super::{SrcExprVariants, SweetExpression};
use crate::{
    context::Context, inference::InferenceExpression, Expression, ExpressionReference, Result,
    Variable, VariableBinding,
};

pub type NamesResolvedExpression<'src> = SweetExpression<'src, Variable<'src>>;

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
