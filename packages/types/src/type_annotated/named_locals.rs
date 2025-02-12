use context::VariableLookup;

use super::ExprVariants;
use crate::{
    context::Context, inference, type_annotated, type_annotated::indexed_locals, GenericExpression,
    Result, VariableBinding,
};

mod context;

pub type Expression<'src> = type_annotated::Expression<'src, &'src str>;

impl<'src> Expression<'src> {
    pub fn variable(name: &'src str) -> Self {
        Self::new(ExprVariants::Variable(name))
    }

    pub fn resolve_names(&self) -> Result<indexed_locals::Expression<'src>> {
        self.resolve_names_with_lookup(&mut VariableLookup::default())
    }

    pub fn link(&self, ctx: &mut Context<'src>) -> Result<inference::Expression<'src>> {
        self.resolve_names()?.link(ctx)
    }

    fn resolve_names_with_lookup(
        &self,
        lookup: &mut VariableLookup<'src>,
    ) -> Result<indexed_locals::Expression<'src>> {
        Ok(match self.0.as_ref() {
            ExprVariants::Known { expression } => expression.resolve_names(lookup)?,
            ExprVariants::Variable(name) => {
                indexed_locals::Expression::new(ExprVariants::Variable(lookup.lookup(name)))
            }
            ExprVariants::TypeAnnotation { expression, typ } => {
                let expression = expression.resolve_names_with_lookup(lookup)?;
                let typ = typ.resolve_names_with_lookup(lookup)?;
                indexed_locals::Expression::new(ExprVariants::TypeAnnotation { expression, typ })
            }
            ExprVariants::Unknown { typ } => {
                let typ = typ.resolve_names_with_lookup(lookup)?;
                indexed_locals::Expression::new(ExprVariants::Unknown { typ })
            }
        })
    }
}

impl<'src> GenericExpression<'src, Expression<'src>> {
    fn resolve_names(
        &self,
        lookup: &mut VariableLookup<'src>,
    ) -> Result<indexed_locals::Expression<'src>> {
        let expr = match self {
            Self::TypeOfType => GenericExpression::TypeOfType,
            Self::Constant { name, typ } => GenericExpression::Constant {
                name,
                typ: typ.resolve_names()?,
            },
            Self::Apply {
                function,
                argument,
                typ,
            } => GenericExpression::Apply {
                function: function.resolve_names_with_lookup(lookup)?,
                argument: argument.resolve_names_with_lookup(lookup)?,
                typ: typ.resolve_names_with_lookup(lookup)?,
            },
            Self::VariableBinding(binding) => {
                GenericExpression::VariableBinding(binding.resolve_names(lookup)?)
            }
        };

        Ok(indexed_locals::Expression::new(ExprVariants::Known {
            expression: expr,
        }))
    }
}

impl<'src> VariableBinding<'src, Expression<'src>> {
    fn resolve_names(
        &self,
        lookup: &mut VariableLookup<'src>,
    ) -> Result<VariableBinding<'src, indexed_locals::Expression<'src>>> {
        let variable_value = self.variable_value.resolve_names_with_lookup(lookup)?;

        let in_expression = if let Some(name) = self.name {
            lookup.with_variable(name, |lookup| {
                self.in_expression.resolve_names_with_lookup(lookup)
            })
        } else {
            self.in_expression.resolve_names_with_lookup(lookup)
        }?;

        Ok(VariableBinding {
            name: self.name,
            binder: self.binder,
            variable_value,
            in_expression,
        })
    }
}
