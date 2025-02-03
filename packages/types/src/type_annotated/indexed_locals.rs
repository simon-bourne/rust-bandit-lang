use super::ExprVariants;
use crate::{
    context::Context, inference, type_annotated, ExpressionReference, GenericExpression, Result,
    Variable, VariableBinding,
};

pub type Expression<'src> = type_annotated::Expression<'src, Variable<'src>>;

impl<'src> Expression<'src> {
    pub fn link(&self, ctx: &mut Context<'src>) -> Result<inference::Expression<'src>> {
        Ok(match self.0.as_ref() {
            ExprVariants::Known { expression } => expression.link(ctx)?,
            ExprVariants::TypeAnnotation { expression, typ } => {
                let expression = expression.link(ctx)?;
                let mut typ = typ.link(ctx)?;
                inference::Expression::unify(&mut expression.typ(), &mut typ)?;
                expression
            }
            ExprVariants::Unknown { typ } => inference::Expression::unknown(typ.link(ctx)?),
        })
    }
}

impl<'src> GenericExpression<'src, Expression<'src>> {
    fn link(&self, ctx: &mut Context<'src>) -> Result<inference::Expression<'src>> {
        Ok(inference::Expression::new(match self {
            Self::TypeOfType => GenericExpression::TypeOfType,
            Self::Constant { name, typ } => GenericExpression::Constant {
                name,
                typ: typ.link(ctx)?,
            },
            Self::Apply {
                function,
                argument,
                typ,
            } => GenericExpression::Apply {
                function: function.link(ctx)?,
                argument: argument.link(ctx)?,
                typ: typ.link(ctx)?,
            },
            Self::Let(binding) => GenericExpression::Let(binding.link(ctx)?),
            Self::Pi(binding) => GenericExpression::Pi(binding.link(ctx)?),
            Self::Lambda(binding) => GenericExpression::Lambda(binding.link(ctx)?),
            Self::Variable(variable) => return ctx.lookup_value(*variable),
        }))
    }
}

impl<'src> VariableBinding<'src, Expression<'src>> {
    fn link(
        &self,
        ctx: &mut Context<'src>,
    ) -> Result<VariableBinding<'src, inference::Expression<'src>>> {
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
