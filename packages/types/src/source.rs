use std::rc::Rc;

use super::pretty::{Document, Layout, Operator, Side};
use crate::{
    context::Context, inference, pretty::TypeAnnotated, Binder, ExpressionReference,
    GenericExpression, Pretty, Result, VariableBinding,
};

#[derive(Clone)]
pub struct Expression<'src>(Rc<ExprVariants<'src>>);

impl<'src> ExpressionReference<'src> for Expression<'src> {
    fn is_known(&self) -> bool {
        self.0.is_known()
    }

    fn typ(&self) -> Self {
        self.0.typ(Self::known)
    }
}

impl Pretty for Expression<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        match self.0.as_ref() {
            ExprVariants::Known { expression } => expression.to_document(parent, layout),
            ExprVariants::Variable(variable) => variable.to_document(parent, layout),
            ExprVariants::TypeAnnotation { expression, typ } => {
                TypeAnnotated::new(expression, typ).to_document(parent, layout)
            }
            ExprVariants::Unknown { typ } => {
                TypeAnnotated::new(None, typ).to_document(parent, layout)
            }
        }
    }
}

impl<'src> Expression<'src> {
    pub fn type_of_type() -> Self {
        Self::known(GenericExpression::TypeOfType)
    }

    pub fn unknown_value() -> Self {
        Self::new(ExprVariants::Unknown {
            typ: Self::unknown_type(),
        })
    }

    pub fn unknown_type() -> Self {
        Self::new(ExprVariants::Unknown {
            typ: Self::type_of_type(),
        })
    }

    pub fn type_constant(name: &'src str) -> Self {
        Self::known(GenericExpression::Constant {
            name,
            typ: Self::type_of_type(),
        })
    }

    pub fn constant(name: &'src str, typ: Self) -> Self {
        Self::known(GenericExpression::Constant { name, typ })
    }

    pub fn apply(self, argument: Self) -> Self {
        Self::known(GenericExpression::Apply {
            function: self,
            argument,
            typ: Self::unknown_type(),
        })
    }

    pub fn let_binding(name: &'src str, variable_value: Self, in_expression: Self) -> Self {
        Self::binding(Binder::Let, Some(name), variable_value, in_expression)
    }

    pub fn pi_type(name: Option<&'src str>, variable_value: Self, result_type: Self) -> Self {
        Self::binding(Binder::Pi, name, variable_value, result_type)
    }

    pub fn lambda(name: &'src str, variable_value: Self, in_expression: Self) -> Self {
        Self::binding(Binder::Lambda, Some(name), variable_value, in_expression)
    }

    pub fn has_type(self, typ: Self) -> Self {
        Self::new(ExprVariants::TypeAnnotation {
            expression: self,
            typ,
        })
    }

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

    fn new(expr: ExprVariants<'src>) -> Self {
        Self(Rc::new(expr))
    }

    fn known(expression: GenericExpression<'src, Self>) -> Self {
        Self::new(ExprVariants::Known { expression })
    }

    fn binding(
        binder: Binder,
        name: Option<&'src str>,
        variable_value: Self,
        in_expression: Self,
    ) -> Self {
        Self::known(GenericExpression::VariableBinding(VariableBinding {
            name,
            binder,
            variable_value,
            in_expression,
        }))
    }
}

enum ExprVariants<'src> {
    Known {
        expression: GenericExpression<'src, Expression<'src>>,
    },
    TypeAnnotation {
        expression: Expression<'src>,
        typ: Expression<'src>,
    },
    Unknown {
        typ: Expression<'src>,
    },
    Variable(&'src str),
}

impl<'src> ExprVariants<'src> {
    fn is_known(&self) -> bool {
        match self {
            Self::Known { .. } | Self::Variable(_) => true,
            Self::TypeAnnotation { expression, .. } => expression.is_known(),
            Self::Unknown { .. } => false,
        }
    }

    fn typ(
        &self,
        new: impl FnOnce(GenericExpression<'src, Expression<'src>>) -> Expression<'src>,
    ) -> Expression<'src> {
        match self {
            Self::Known { expression } => expression.typ(new),
            Self::Variable(_) => Expression::unknown_type(),
            Self::TypeAnnotation { typ, .. } | Self::Unknown { typ, .. } => typ.clone(),
        }
    }
}

impl<'src> GenericExpression<'src, Expression<'src>> {
    fn link(&self, ctx: &mut Context<'src>) -> Result<inference::Expression<'src>> {
        Ok(inference::Expression::new_known(
            0,
            match self {
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
            },
        ))
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
