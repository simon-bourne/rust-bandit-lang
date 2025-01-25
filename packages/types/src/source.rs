use std::rc::Rc;

use crate::{
    context::VariableLookup, Annotation, Document, EmptyName, Expression, ExpressionRef, Inference,
    Pretty, Result, VariableBinding,
};

pub struct Source;

impl<'src> Annotation<'src> for Source {
    type Expression = SourceExpression<'src>;
    type VariableIndex = &'src str;
    type VariableName = &'src str;
}

#[derive(Clone)]
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
            infix: false,
        })
    }

    pub fn apply_operator(function: Self, argument: Self, typ: Self) -> Self {
        Self::new(Expression::Apply {
            function,
            argument,
            typ,
            infix: true,
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
    fn to_document(&self) -> Document {
        match self.0.as_ref() {
            Some(expr) => expr.to_document(),
            None => Document::text("{unknown}"),
        }
    }

    fn type_annotatation(&self, term: Document, parenthesized: bool) -> Document {
        match self.0.as_ref() {
            Some(expr) => expr.type_annotatation(term, parenthesized),
            None => term,
        }
    }
}

impl<'src> Expression<'src, Source> {
    fn to_infer(&self, lookup: &mut VariableLookup<'src>) -> Result<ExpressionRef<'src>> {
        let expr = match self {
            Self::Type => Expression::Type,
            Self::Apply {
                function,
                argument,
                typ,
                infix,
            } => Expression::Apply {
                function: function.to_infer_with_lookup(lookup)?,
                argument: argument.to_infer_with_lookup(lookup)?,
                typ: typ.to_infer_with_lookup(lookup)?,
                infix: *infix,
            },
            Self::Let {
                variable_value,
                binding,
            } => {
                let variable_value = variable_value.to_infer_with_lookup(lookup)?;
                Expression::Let {
                    variable_value,
                    binding: binding.to_infer(lookup)?,
                }
            }
            Self::FunctionType(binding) => Expression::FunctionType(binding.to_infer(lookup)?),
            Self::Lambda(binding) => Expression::Lambda(binding.to_infer(lookup)?),
            Self::Variable { index, typ } => Expression::Variable {
                index: lookup.lookup(index),
                typ: typ.to_infer_with_lookup(lookup)?,
            },
        };

        Ok(ExpressionRef::new(expr))
    }
}

impl<'src> VariableBinding<'src, Source> {
    fn to_infer(
        &self,
        lookup: &mut VariableLookup<'src>,
    ) -> Result<VariableBinding<'src, Inference>> {
        let variable_type = self.variable_type.to_infer_with_lookup(lookup)?;

        lookup.with_variable(self.name, |lookup| {
            let in_expression = self.in_expression.to_infer_with_lookup(lookup)?;
            Ok(VariableBinding {
                name: EmptyName,
                variable_type,
                in_expression,
            })
        })
    }
}
