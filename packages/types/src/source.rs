use std::rc::Rc;

use crate::{
    context::VariableLookup,
    pretty::{disambiguate, Document, Operator, Side, TypeAnnotations},
    EmptyName, Expression, ExpressionRef, Inference, Pretty, Result, Stage, VariableBinding,
};

pub struct Source;

impl<'src> Stage<'src> for Source {
    type Expression = SourceExpression<'src>;
    type VariableIndex = &'src str;
    type VariableName = &'src str;
}

#[derive(Clone)]
pub struct SourceExpression<'src>(Rc<SrcExprVariants<'src>>);

enum SrcExprVariants<'src> {
    Known {
        expression: Expression<'src, Source>,
    },
    TypeAnnotation {
        expression: SourceExpression<'src>,
        typ: SourceExpression<'src>,
    },
    Unknown {
        typ: SourceExpression<'src>,
    },
}

impl<'src> SourceExpression<'src> {
    pub fn unknown_term() -> Self {
        Self(Rc::new(SrcExprVariants::Unknown {
            typ: Self::unknown_type(),
        }))
    }

    pub fn unknown_type() -> Self {
        Self(Rc::new(SrcExprVariants::Unknown {
            typ: Self::type_of_type(),
        }))
    }

    pub fn type_of_type() -> Self {
        Self::new(Expression::Type)
    }

    pub fn apply(self, argument: Self) -> Self {
        Self::new(Expression::Apply {
            function: self,
            argument,
            typ: Self::unknown_type(),
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

    pub fn variable(index: &'src str) -> Self {
        Self::new(Expression::Variable {
            index,
            typ: Self::unknown_type(),
        })
    }

    pub fn has_type(self, typ: Self) -> Self {
        Self(Rc::new(SrcExprVariants::TypeAnnotation {
            expression: self,
            typ,
        }))
    }

    pub fn to_infer(&self) -> Result<ExpressionRef<'src>> {
        self.to_infer_with_lookup(&mut VariableLookup::default())
    }

    fn to_infer_with_lookup(
        &self,
        lookup: &mut VariableLookup<'src>,
    ) -> Result<ExpressionRef<'src>> {
        Ok(match self.0.as_ref() {
            SrcExprVariants::Known { expression } => expression.to_infer(lookup)?,
            SrcExprVariants::TypeAnnotation { expression, typ } => {
                let expression = expression.to_infer_with_lookup(lookup)?;
                let typ = &mut typ.to_infer_with_lookup(lookup)?;
                ExpressionRef::unify(&mut expression.typ(), typ)?;
                expression
            }
            SrcExprVariants::Unknown { typ } => {
                ExpressionRef::unknown(typ.to_infer_with_lookup(lookup)?)
            }
        })
    }

    fn operator(&self) -> Option<Operator> {
        match self.0.as_ref() {
            SrcExprVariants::Known { expression } => match expression {
                Expression::Apply { .. } => Some(Operator::Apply),
                Expression::FunctionType(binding) => {
                    (binding.name == "_").then_some(Operator::Arrow)
                }
                Expression::Type
                | Expression::Let { .. }
                | Expression::Lambda(_)
                | Expression::Variable { .. } => None,
            },
            SrcExprVariants::TypeAnnotation { .. } => Some(Operator::HasType),
            SrcExprVariants::Unknown { .. } => None,
        }
    }

    fn new(expression: Expression<'src, Source>) -> Self {
        Self(Rc::new(SrcExprVariants::Known { expression }))
    }
}

impl Pretty for SourceExpression<'_> {
    fn to_document(
        &self,
        parent: Option<(Operator, Side)>,
        type_annotations: TypeAnnotations,
    ) -> Document {
        match self.0.as_ref() {
            SrcExprVariants::Known { expression } => {
                expression.to_document(parent, type_annotations)
            }
            SrcExprVariants::TypeAnnotation { expression, typ } => {
                let op = Operator::HasType;
                let term = expression.to_document(Some((op, Side::Left)), type_annotations);
                typ.type_annotatation(term, expression.operator(), parent, type_annotations)
            }
            SrcExprVariants::Unknown { typ } => {
                typ.type_annotatation(Document::text("_"), None, parent, type_annotations)
            }
        }
    }

    fn type_annotatation(
        &self,
        term: Document,
        term_operator: Option<Operator>,
        parent: Option<(Operator, Side)>,
        type_annotations: TypeAnnotations,
    ) -> Document {
        match self.0.as_ref() {
            SrcExprVariants::Known { expression } => {
                expression.type_annotatation(term, term_operator, parent, type_annotations)
            }
            SrcExprVariants::TypeAnnotation { .. } => {
                let op = Operator::HasType;
                let typ = self.to_document(Some((op, Side::Left)), type_annotations);
                disambiguate(
                    Some(op),
                    parent,
                    [
                        disambiguate(term_operator, Some((op, Side::Right)), [term]),
                        Document::text(":"),
                        typ,
                    ],
                )
            }
            SrcExprVariants::Unknown { .. } => term,
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
            } => Expression::Apply {
                function: function.to_infer_with_lookup(lookup)?,
                argument: argument.to_infer_with_lookup(lookup)?,
                typ: typ.to_infer_with_lookup(lookup)?,
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
