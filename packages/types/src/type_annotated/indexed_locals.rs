use super::ExprVariants;
use crate::{
    context::{Context, Link},
    inference, type_annotated, ExpressionReference, GenericExpression, Result, Variable,
    VariableBinding,
};

pub type Expression<'src> = type_annotated::Expression<'src, Variable<'src>>;

impl<'src> Expression<'src> {
    pub fn link(&self, ctx: &mut Context<'src>) -> Result<inference::Expression<'src>> {
        Ok(match self.0.as_ref() {
            ExprVariants::Known { expression } => expression.link(ctx)?,
            ExprVariants::Variable(variable) => ctx.lookup_value(*variable)?,
            ExprVariants::TypeAnnotation { expression, typ } => {
                let expression = expression.link(ctx)?;
                let mut typ = typ.link(ctx)?;
                inference::Expression::unify(&mut expression.typ(), &mut typ)?;
                expression
            }
            ExprVariants::LinkedUnknown { expression, typ } => {
                inference::Expression::unify(&mut expression.typ(), &mut typ.link(ctx)?)?;
                expression.clone()
            }
            ExprVariants::FreshUnknown { typ } => inference::Expression::unknown(typ.link(ctx)?),
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
            Self::Let(binding) => GenericExpression::Let(binding.link(ctx, None)?),
            Self::Pi(binding) => GenericExpression::Pi(binding.link(ctx, Some(Link::Now))?),
            Self::Lambda(binding) => GenericExpression::Lambda(binding.link(ctx, None)?),
        }))
    }
}

impl<'src> VariableBinding<'src, Expression<'src>> {
    fn link(
        &self,
        ctx: &mut Context<'src>,
        link: Option<Link>,
    ) -> Result<VariableBinding<'src, inference::Expression<'src>>> {
        // TODO: Not sure about this `Link` stuff
        let link = link.unwrap_or_else(|| {
            if self.variable_value.is_type_annotated() {
                Link::OnLookup
            } else {
                Link::Now
            }
        });

        ctx.with_variable(self.variable_value.clone(), link, |ctx, variable_value| {
            Ok(VariableBinding {
                name: self.name,
                variable_value,
                in_expression: self.in_expression.link(ctx)?,
            })
        })?
    }
}
