use super::ExprVariants;
use crate::{
    context::Context,
    inference,
    type_annotated::{self},
    ExpressionReference, GenericExpression, Result, VariableBinding,
};

pub type Expression<'src> = type_annotated::Expression<'src, &'src str>;

impl<'src> Expression<'src> {
    pub fn variable(name: &'src str) -> Self {
        Self::new(ExprVariants::Variable(name))
    }

    pub fn link(&self, ctx: &mut Context<'src>) -> Result<inference::Expression<'src>> {
        Ok(match self.0.as_ref() {
            ExprVariants::Known { expression } => expression.link(ctx)?,
            ExprVariants::Variable(name) => ctx.lookup(name)?,
            ExprVariants::TypeAnnotation { expression, typ } => {
                let expression = expression.link(ctx)?;
                let typ = &mut typ.link(ctx)?;
                inference::Expression::unify(&mut expression.typ(), typ)?;
                expression
            }
            ExprVariants::Unknown { typ } => inference::Expression::unknown(typ.link(ctx)?),
        })
    }
}

impl<'src> GenericExpression<'src, Expression<'src>> {
    pub fn link(&self, ctx: &mut Context<'src>) -> Result<inference::Expression<'src>> {
        Ok(inference::Expression::new_known(match self {
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
            Self::VariableBinding(binding) => {
                GenericExpression::VariableBinding(binding.link(ctx)?)
            }
        }))
    }
}

impl<'src> VariableBinding<'src, Expression<'src>> {
    fn link(
        &self,
        ctx: &mut Context<'src>,
    ) -> Result<VariableBinding<'src, inference::Expression<'src>>> {
        let mut variable_value = self.variable_value.link(ctx)?;

        let in_expression = if let Some(name) = self.name {
            variable_value.set_name(name);

            ctx.with_variable(name, variable_value.clone(), |ctx| {
                self.in_expression.link(ctx)
            })?
        } else {
            self.in_expression.link(ctx)
        }?;

        Ok(VariableBinding {
            name: self.name,
            binder: self.binder,
            variable_value,
            in_expression,
        })
    }
}
