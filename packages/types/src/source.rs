use std::rc::Rc;

use context::VariableLookup;

use super::{
    context::Context,
    pretty::{Annotation, Document, Operator, Side},
};
use crate::{
    inference::InferenceExpression, Expression, ExpressionReference, Pretty, Result, Variable,
    VariableBinding,
};

mod context;

impl<'src> ExpressionReference<'src> for SourceExpression<'src> {
    type Variable = &'src str;
}

impl<'src> ExpressionReference<'src> for NamesResolvedExpression<'src> {
    type Variable = Variable<'src>;
}

// TODO: Make private
type SweetExpression<'src, Expr> = Rc<SrcExprVariants<'src, Expr>>;

#[derive(Clone)]
pub struct SourceExpression<'src>(SweetExpression<'src, Self>);

#[derive(Clone)]
pub struct NamesResolvedExpression<'src>(SweetExpression<'src, Self>);

impl<'src> NamesResolvedExpression<'src> {
    fn new(expr: SrcExprVariants<'src, Self>) -> Self {
        Self(Rc::new(expr))
    }
}

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

enum SrcExprVariants<'src, Expr: ExpressionReference<'src>> {
    Known { expression: Expression<'src, Expr> },
    TypeAnnotation { expression: Expr, typ: Expr },
    Unknown { typ: Expr },
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

    pub fn unknown_with_type(typ: Self) -> Self {
        Self(Rc::new(SrcExprVariants::Unknown { typ }))
    }

    pub fn type_constant(name: &'src str) -> Self {
        Self::new(Expression::Constant {
            name,
            typ: Self::type_of_type(),
        })
    }

    pub fn constant(name: &'src str, typ: Self) -> Self {
        Self::new(Expression::Constant { name, typ })
    }

    pub fn type_of_type() -> Self {
        Self::new(Expression::TypeOfType)
    }

    pub fn apply(self, argument: Self) -> Self {
        Self::new(Expression::Apply {
            function: self,
            argument,
            typ: Self::unknown_type(),
        })
    }

    pub fn let_binding(name: &'src str, variable_value: Self, in_expression: Self) -> Self {
        Self::new(Expression::Let(VariableBinding {
            name,
            variable_value,
            in_expression,
        }))
    }

    pub fn function_type(name: &'src str, argument_type: Self, result_type: Self) -> Self {
        Self::new(Expression::FunctionType(VariableBinding {
            name,
            variable_value: Self::unknown_with_type(argument_type),
            in_expression: result_type,
        }))
    }

    pub fn lambda(name: &'src str, argument_type: Self, in_expression: Self) -> Self {
        Self::new(Expression::Lambda(VariableBinding {
            name,
            variable_value: Self::unknown_with_type(argument_type),
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

    pub fn link(&self, ctx: &mut Context<'src>) -> Result<InferenceExpression<'src>> {
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

    fn new(expression: Expression<'src, Self>) -> Self {
        Self(Rc::new(SrcExprVariants::Known { expression }))
    }
}

impl<'src> Expression<'src, SourceExpression<'src>> {
    fn resolve_names(
        &self,
        lookup: &mut VariableLookup<'src>,
    ) -> Result<NamesResolvedExpression<'src>> {
        let expr = match self {
            Self::TypeOfType => Expression::TypeOfType,
            Self::Constant { name, typ } => Expression::Constant {
                name,
                typ: typ.resolve_names()?,
            },
            Self::Apply {
                function,
                argument,
                typ,
            } => Expression::Apply {
                function: function.resolve_names_with_lookup(lookup)?,
                argument: argument.resolve_names_with_lookup(lookup)?,
                typ: typ.resolve_names_with_lookup(lookup)?,
            },
            Self::Let(binding) => Expression::Let(binding.resolve_names(lookup)?),
            Self::FunctionType(binding) => Expression::FunctionType(binding.resolve_names(lookup)?),
            Self::Lambda(binding) => Expression::Lambda(binding.resolve_names(lookup)?),
            Self::Variable(name) => Expression::Variable(lookup.lookup(name)),
        };

        Ok(NamesResolvedExpression::new(SrcExprVariants::Known {
            expression: expr,
        }))
    }
}

impl<'src> VariableBinding<'src, SourceExpression<'src>> {
    fn resolve_names(
        &self,
        lookup: &mut VariableLookup<'src>,
    ) -> Result<VariableBinding<'src, NamesResolvedExpression<'src>>> {
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

impl Pretty for SourceExpression<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotations: Annotation) -> Document {
        self.0.to_document(parent, annotations)
    }

    // TODO: Have a `typ()` method instead
    fn type_to_document(&self, parent: Option<(Operator, Side)>) -> Document {
        self.0.type_to_document(parent)
    }

    fn is_known(&self) -> bool {
        self.0.is_known()
    }

    fn type_is_known(&self) -> bool {
        self.0.type_is_known()
    }
}

impl Pretty for NamesResolvedExpression<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotations: Annotation) -> Document {
        self.0.to_document(parent, annotations)
    }

    fn type_to_document(&self, parent: Option<(Operator, Side)>) -> Document {
        self.0.type_to_document(parent)
    }

    fn is_known(&self) -> bool {
        self.0.is_known()
    }

    fn type_is_known(&self) -> bool {
        self.0.type_is_known()
    }
}

impl<'src, S> Pretty for SweetExpression<'src, S>
where
    S: ExpressionReference<'src>,
{
    fn to_document(&self, parent: Option<(Operator, Side)>, annotation: Annotation) -> Document {
        match self.as_ref() {
            SrcExprVariants::Known { expression } => expression.to_document(parent, annotation),
            SrcExprVariants::TypeAnnotation { expression, typ } => {
                expression.annotate_with_type(typ, parent, annotation)
            }
            SrcExprVariants::Unknown { typ } => "_".annotate_with_type(typ, parent, annotation),
        }
    }

    fn type_to_document(&self, parent: Option<(Operator, Side)>) -> Document {
        match self.as_ref() {
            SrcExprVariants::Known { expression } => expression.type_to_document(parent),
            SrcExprVariants::TypeAnnotation { typ, .. } => typ.to_document(parent, Annotation::Off),
            SrcExprVariants::Unknown { typ } => typ.to_document(parent, Annotation::Off),
        }
    }

    fn is_known(&self) -> bool {
        match self.as_ref() {
            SrcExprVariants::Known { expression } => expression.is_known(),
            SrcExprVariants::TypeAnnotation { expression, .. } => expression.is_known(),
            SrcExprVariants::Unknown { .. } => false,
        }
    }

    fn type_is_known(&self) -> bool {
        match self.as_ref() {
            SrcExprVariants::Known { expression } => expression.type_is_known(),
            SrcExprVariants::TypeAnnotation { typ, .. } => typ.is_known(),
            SrcExprVariants::Unknown { typ } => typ.is_known(),
        }
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
            // TODO: Don't return
            Self::Variable(variable) => return ctx.lookup_value(*variable),
        }))
    }
}

impl<'src> VariableBinding<'src, NamesResolvedExpression<'src>> {
    fn link(&self, ctx: &mut Context<'src>) -> Result<VariableBinding<'src, InferenceExpression<'src>>> {
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
