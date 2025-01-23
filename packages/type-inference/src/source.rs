use std::rc::Rc;

use crate::{
    context::VariableLookup, Annotation, Expression, ExpressionRef, InferenceError, Pretty,
    PrettyDoc, Result, VariableBinding,
};

pub struct Source;

impl<'src> Annotation<'src> for Source {
    type Expression = SourceExpression<'src>;
    type VariableIndex = &'src str;
    type VariableName = &'src str;
}

pub struct SourceExpression<'src>(Option<Rc<Expression<'src, Source>>>);

impl<'src> SourceExpression<'src> {
    pub fn unknown() -> Self {
        Self(None)
    }

    pub fn type_of_type() -> Self {
        Self::new(Expression::Type)
    }

    pub fn apply(function: Self, argument: Self, typ: Self) -> Self {
        Self::new(Expression::Apply {
            function,
            argument,
            typ,
        })
    }

    pub fn let_binding(
        name: &'src str,
        variable_type: Self,
        variable_value: Self,
        in_expression: Self,
    ) -> Self {
        Self::new(Expression::Let {
            variable_value,
            binding: VariableBinding {
                name,
                variable_type,
                in_expression,
            },
        })
    }

    pub fn function_type(name: &'src str, argument_type: Self, result_type: Self) -> Self {
        Self::new(Expression::FunctionType(VariableBinding {
            name,
            variable_type: argument_type,
            in_expression: result_type,
        }))
    }

    pub fn lambda(name: &'src str, argument_type: Self, in_expression: Self) -> Self {
        Self::new(Expression::Lambda(VariableBinding {
            name,
            variable_type: argument_type,
            in_expression,
        }))
    }

    pub fn variable(index: &'src str, typ: Self) -> Self {
        Self::new(Expression::Variable { index, typ })
    }

    pub fn to_infer(&self) -> Result<ExpressionRef<'src>> {
        self.to_infer_with_lookup(&mut VariableLookup::default())
    }

    fn to_infer_with_lookup(
        &self,
        lookup: &mut VariableLookup<'src>,
    ) -> Result<ExpressionRef<'src>> {
        match self.0.as_ref() {
            Some(expr) => expr.to_infer(lookup),
            None => Ok(ExpressionRef::unknown()),
        }
    }

    fn new(expr: Expression<'src, Source>) -> Self {
        Self(Some(Rc::new(expr)))
    }
}

impl Pretty for SourceExpression<'_> {
    fn pretty(&self) -> PrettyDoc {
        match self.0.as_ref() {
            Some(expr) => expr.pretty(),
            None => PrettyDoc::text("{unknown}"),
        }
    }
}

impl<'src> Expression<'src, Source> {
    fn to_infer(&self, lookup: &mut VariableLookup<'src>) -> Result<ExpressionRef<'src>> {
        Ok(match self {
            Self::Type => ExpressionRef::type_of_type(),
            Self::Apply {
                function,
                argument,
                typ,
            } => ExpressionRef::apply(
                function.to_infer_with_lookup(lookup)?,
                argument.to_infer_with_lookup(lookup)?,
                typ.to_infer_with_lookup(lookup)?,
            ),
            Self::Let {
                variable_value,
                binding,
            } => {
                let variable_type = binding.variable_type.to_infer_with_lookup(lookup)?;
                let variable_value = variable_value.to_infer_with_lookup(lookup)?;
                lookup.with_variable(binding.name, |lookup| {
                    Ok(ExpressionRef::let_binding(
                        variable_type,
                        variable_value,
                        binding.in_expression.to_infer_with_lookup(lookup)?,
                    ))
                })?
            }
            Self::FunctionType(binding) => {
                let variable_type = binding.variable_type.to_infer_with_lookup(lookup)?;
                lookup.with_variable(binding.name, |lookup| {
                    Ok(ExpressionRef::function_type(
                        variable_type,
                        binding.in_expression.to_infer_with_lookup(lookup)?,
                    ))
                })?
            }
            Self::Lambda(binding) => lookup.with_variable(binding.name, |lookup| {
                let variable_type = binding.variable_type.to_infer_with_lookup(lookup)?;
                Ok(ExpressionRef::lambda(
                    variable_type,
                    binding.in_expression.to_infer_with_lookup(lookup)?,
                ))
            })?,
            Self::Variable { index, typ } => ExpressionRef::variable(
                lookup.lookup(index).ok_or(InferenceError)?,
                typ.to_infer_with_lookup(lookup)?,
            ),
        })
    }
}
