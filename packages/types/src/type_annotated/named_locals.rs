use context::VariableLookup;

use super::SrcExprVariants;
use crate::{
    context::Context, inference, type_annotated, type_annotated::indexed_locals, GenericExpression,
    Result, VariableBinding,
};

mod context;

pub type Expression<'src> = type_annotated::Expression<'src, &'src str>;

impl<'src> Expression<'src> {
    pub fn variable(name: &'src str) -> Self {
        Self::known(GenericExpression::Variable(name))
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
            SrcExprVariants::Known { expression } => expression.resolve_names(lookup)?,
            SrcExprVariants::TypeAnnotation { expression, typ } => {
                let expression = expression.resolve_names_with_lookup(lookup)?;
                let typ = typ.resolve_names_with_lookup(lookup)?;
                indexed_locals::Expression::new(SrcExprVariants::TypeAnnotation { expression, typ })
            }
            SrcExprVariants::Unknown { typ } => {
                let typ = typ.resolve_names_with_lookup(lookup)?;
                indexed_locals::Expression::new(SrcExprVariants::Unknown { typ })
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
            Self::Let(binding) => GenericExpression::Let(binding.resolve_names(lookup)?),
            Self::FunctionType(binding) => {
                GenericExpression::FunctionType(binding.resolve_names(lookup)?)
            }
            Self::Lambda(binding) => GenericExpression::Lambda(binding.resolve_names(lookup)?),
            Self::Variable(name) => GenericExpression::Variable(lookup.lookup(name)),
        };

        Ok(indexed_locals::Expression::new(SrcExprVariants::Known {
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
