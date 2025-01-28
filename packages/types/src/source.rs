use std::{fmt, rc::Rc};

use crate::{
    context::{Context, Variable, VariableLookup},
    literal::Literal,
    pretty::{disambiguate, Document, Operator, Side, TypeAnnotations},
    Expression, ExpressionRef, Pretty, Result, Stage, VariableBinding,
};

#[derive(Clone)]
pub struct Source;

impl<'src> Stage<'src> for Source {
    type Expression = SourceExpression<'src>;
    type Variable = &'src str;
}

#[derive(Clone)]
pub struct NamesResolved;

impl<'src> Stage<'src> for NamesResolved {
    type Expression = NamesResolvedExpression<'src>;
    type Variable = Variable<'src>;
}

#[derive(Clone)]
pub struct SweetExpression<'src, S: Stage<'src>>(Rc<SrcExprVariants<'src, S>>);

pub type SourceExpression<'src> = SweetExpression<'src, Source>;

pub type NamesResolvedExpression<'src> = SweetExpression<'src, NamesResolved>;

impl<'src> NamesResolvedExpression<'src> {
    fn new(expr: SrcExprVariants<'src, NamesResolved>) -> Self {
        Self(Rc::new(expr))
    }
}

impl<'src> NamesResolvedExpression<'src> {
    pub fn link(&self, ctx: &mut Context<'src>) -> Result<ExpressionRef<'src>> {
        Ok(match self.0.as_ref() {
            SrcExprVariants::Known { expression } => expression.link(ctx)?,
            SrcExprVariants::TypeAnnotation { expression, typ } => {
                let expression = expression.link(ctx)?;
                let mut typ = typ.link(ctx)?;
                ExpressionRef::unify(&mut expression.typ(), &mut typ)?;
                expression
            }
            SrcExprVariants::Unknown { typ } => ExpressionRef::unknown(typ.link(ctx)?),
        })
    }
}

enum SrcExprVariants<'src, T: Stage<'src>> {
    Known {
        expression: Expression<'src, T>,
    },
    TypeAnnotation {
        expression: T::Expression,
        typ: T::Expression,
    },
    Unknown {
        typ: T::Expression,
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
        Self::new(Expression::Literal(Literal::TypeOfType))
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

    pub fn variable(name: &'src str) -> Self {
        Self::new(Expression::Variable(name))
    }

    pub fn has_type(self, typ: Self) -> Self {
        Self(Rc::new(SrcExprVariants::TypeAnnotation {
            expression: self,
            typ,
        }))
    }

    pub fn resolve_names(&self) -> Result<NamesResolvedExpression<'src>> {
        self.resolve_names_with_lookup(&mut VariableLookup::default())
    }

    pub fn link(&self, ctx: &mut Context<'src>) -> Result<ExpressionRef<'src>> {
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

    fn new(expression: Expression<'src, Source>) -> Self {
        Self(Rc::new(SrcExprVariants::Known { expression }))
    }
}

trait GetOperator {
    fn operator(&self) -> Option<Operator>;
}

impl<'src, S: Stage<'src>> GetOperator for SweetExpression<'src, S> {
    fn operator(&self) -> Option<Operator> {
        match self.0.as_ref() {
            SrcExprVariants::Known { expression } => match expression {
                Expression::Apply { .. } => Some(Operator::Apply),
                Expression::FunctionType(binding) => {
                    (binding.name == "_").then_some(Operator::Arrow)
                }
                Expression::Literal(_)
                | Expression::Let { .. }
                | Expression::Lambda(_)
                | Expression::Variable { .. } => None,
            },
            SrcExprVariants::TypeAnnotation { .. } => Some(Operator::HasType),
            SrcExprVariants::Unknown { .. } => None,
        }
    }
}

impl<'src, S> fmt::Debug for SweetExpression<'src, S>
where
    S::Expression: GetOperator,
    S: Stage<'src>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.to_pretty_string(80))
    }
}

impl<'src, S> Pretty for SweetExpression<'src, S>
where
    S::Expression: GetOperator,
    S: Stage<'src>,
{
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
                let has_type = Operator::HasType;
                let term = expression.to_document(Some((has_type, Side::Left)), type_annotations);
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
    fn resolve_names(
        &self,
        lookup: &mut VariableLookup<'src>,
    ) -> Result<NamesResolvedExpression<'src>> {
        let expr = match self {
            Self::Literal(literal) => Expression::Literal(literal.clone()),
            Self::Apply {
                function,
                argument,
                typ,
            } => Expression::Apply {
                function: function.resolve_names_with_lookup(lookup)?,
                argument: argument.resolve_names_with_lookup(lookup)?,
                typ: typ.resolve_names_with_lookup(lookup)?,
            },
            Self::Let {
                variable_value,
                binding,
            } => {
                let variable_value = variable_value.resolve_names_with_lookup(lookup)?;
                Expression::Let {
                    variable_value,
                    binding: binding.resolve_names(lookup)?,
                }
            }
            Self::FunctionType(binding) => Expression::FunctionType(binding.resolve_names(lookup)?),
            Self::Lambda(binding) => Expression::Lambda(binding.resolve_names(lookup)?),
            Self::Variable(name) => Expression::Variable(lookup.lookup(name)),
        };

        Ok(NamesResolvedExpression::new(SrcExprVariants::Known {
            expression: expr,
        }))
    }
}

impl<'src> VariableBinding<'src, Source> {
    fn resolve_names(
        &self,
        lookup: &mut VariableLookup<'src>,
    ) -> Result<VariableBinding<'src, NamesResolved>> {
        let variable_type = self.variable_type.resolve_names_with_lookup(lookup)?;

        lookup.with_variable(self.name, |lookup| {
            let in_expression = self.in_expression.resolve_names_with_lookup(lookup)?;
            Ok(VariableBinding {
                name: self.name,
                variable_type,
                in_expression,
            })
        })
    }
}
