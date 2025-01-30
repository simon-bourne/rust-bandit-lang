use std::{
    cell::{RefCell, RefMut},
    fmt,
    ops::ControlFlow,
    rc::Rc,
    result,
};

use context::Context;

pub mod context;
mod pretty;
pub mod source;

pub use pretty::Pretty;
use source::NamesResolved;

type SharedMut<T> = Rc<RefCell<T>>;

pub type Result<T> = result::Result<T, InferenceError>;

#[derive(Debug)]
pub struct InferenceError;

pub trait Stage<'src> {
    type Expression: Pretty;
    type Variable: Pretty;
}

pub struct Inference;

impl<'src> Stage<'src> for Inference {
    type Expression = ExpressionRef<'src>;
    type Variable = VariableReference<'src>;
}

#[derive(Clone)]
pub struct ExpressionRef<'src>(SharedMut<ExprRefVariants<'src>>);

enum ExprRefVariants<'src> {
    Known {
        expression: Expression<'src, Inference>,
    },
    Unknown {
        typ: ExpressionRef<'src>,
    },
    Link {
        target: ExpressionRef<'src>,
    },
}

impl<'src> ExpressionRef<'src> {
    fn unknown(typ: Self) -> Self {
        Self(Rc::new(RefCell::new(ExprRefVariants::Unknown { typ })))
    }

    fn type_of_type() -> Self {
        Self::new(Expression::TypeOfType)
    }

    fn function_type(argument_value: Self, result_type: Self) -> Self {
        Self::new(Expression::FunctionType(VariableBinding {
            name: "_",
            variable_value: argument_value,
            in_expression: result_type,
        }))
    }

    fn variable(name: &'src str, value: Self) -> Self {
        Self::new(Expression::Variable(VariableReference { name, value }))
    }

    fn new(expression: Expression<'src, Inference>) -> Self {
        Self(Rc::new(RefCell::new(ExprRefVariants::Known { expression })))
    }

    pub fn infer_types(&mut self) -> Result<()> {
        match &mut *self.0.borrow_mut() {
            ExprRefVariants::Known { expression } => expression.infer_types(),
            ExprRefVariants::Unknown { typ } => typ.infer_types(),
            ExprRefVariants::Link { target } => target.infer_types(),
        }
    }

    fn follow_links(&mut self) {
        loop {
            let borrowed = self.0.borrow();
            let ExprRefVariants::Link { target } = &*borrowed else {
                return;
            };

            let link = target.clone();
            drop(borrowed);
            *self = link;
        }
    }

    fn unify_non_equal(&mut self, other: &mut Self) -> Result<ControlFlow<()>> {
        let mut borrowed = self.0.borrow_mut();

        match &mut *borrowed {
            ExprRefVariants::Unknown { typ } => {
                Self::unify(typ, &mut other.typ())?;
            }
            ExprRefVariants::Known {
                expression: Expression::Variable(var),
            } => {
                Self::unify(&mut var.value, other)?;
            }
            _ => return Ok(ControlFlow::Continue(())),
        }

        drop(borrowed);
        self.replace_with(other);
        Ok(ControlFlow::Break(()))
    }

    fn unify(x: &mut Self, y: &mut Self) -> Result<()> {
        x.follow_links();
        y.follow_links();

        if Rc::ptr_eq(&x.0, &y.0)
            || x.unify_non_equal(y)?.is_break()
            || y.unify_non_equal(x)?.is_break()
        {
            return Ok(());
        }

        let mut x_ref = x.known().expect("x should be known at this point");
        let mut y_ref = y.known().expect("y should be known at this point");

        // TODO: Can we use mutable borrowing to do the occurs check for us?
        match (&mut *x_ref, &mut *y_ref) {
            (Expression::TypeOfType, Expression::TypeOfType) => (),
            (
                Expression::Constant {
                    name: name0,
                    typ: typ0,
                },
                Expression::Constant {
                    name: name1,
                    typ: typ1,
                },
            ) if name0 == name1 => Self::unify(typ0, typ1)?,
            (
                Expression::Apply {
                    function,
                    argument,
                    typ,
                },
                Expression::Apply {
                    function: function1,
                    argument: argument1,
                    typ: typ1,
                },
            ) => {
                Self::unify(function, function1)?;
                Self::unify(argument, argument1)?;
                Self::unify(typ, typ1)?;
            }
            (Expression::Let(binding0), Expression::Let(binding1)) => {
                VariableBinding::unify(binding0, binding1)?
            }
            (Expression::FunctionType(binding0), Expression::FunctionType(binding1)) => {
                VariableBinding::unify(binding0, binding1)?
            }
            (Expression::Lambda(binding0), Expression::Lambda(binding1)) => {
                VariableBinding::unify(binding0, binding1)?
            }
            // It's safer to explicitly ignore each variant
            (Expression::TypeOfType, _rhs)
            | (Expression::Constant { .. }, _rhs)
            | (Expression::Apply { .. }, _rhs)
            | (Expression::Let { .. }, _rhs)
            | (Expression::FunctionType(_), _rhs)
            | (Expression::Lambda(_), _rhs)
            | (Expression::Variable(_), _rhs) => Err(InferenceError)?,
        }

        drop(x_ref);
        drop(y_ref);
        x.replace_with(y);

        Ok(())
    }

    fn replace_with(&mut self, other: &Self) {
        RefCell::replace(
            &self.0,
            ExprRefVariants::Link {
                target: other.clone(),
            },
        );
        *self = other.clone();
    }

    fn known<'a>(&'a self) -> Option<RefMut<'a, Expression<'src, Inference>>> {
        RefMut::filter_map(self.0.borrow_mut(), |x| {
            if let ExprRefVariants::Known { expression } = x {
                Some(expression)
            } else {
                None
            }
        })
        .ok()
    }

    fn typ(&self) -> Self {
        match &*self.0.borrow() {
            ExprRefVariants::Known { expression } => expression.typ(),
            ExprRefVariants::Unknown { typ } => typ.clone(),
            ExprRefVariants::Link { target } => target.typ(),
        }
    }
}

enum Expression<'src, S: Stage<'src>> {
    TypeOfType,
    Constant {
        name: &'src str,
        typ: S::Expression,
    },
    Apply {
        function: S::Expression,
        argument: S::Expression,
        typ: S::Expression,
    },
    Let(VariableBinding<'src, S>),
    FunctionType(VariableBinding<'src, S>),
    Lambda(VariableBinding<'src, S>),
    Variable(S::Variable),
}

struct VariableBinding<'src, S: Stage<'src>> {
    name: &'src str,
    variable_value: S::Expression,
    in_expression: S::Expression,
}

impl VariableBinding<'_, Inference> {
    fn infer_types(&mut self) -> Result<()> {
        // TODO: Is this required?
        ExpressionRef::unify(
            &mut self.variable_value.typ().typ(),
            &mut ExpressionRef::type_of_type(),
        )?;
        self.variable_value.infer_types()?;
        self.in_expression.infer_types()
    }

    fn unify(binding0: &mut Self, binding1: &mut Self) -> Result<()> {
        ExpressionRef::unify(&mut binding0.variable_value, &mut binding1.variable_value)?;
        ExpressionRef::unify(&mut binding0.in_expression, &mut binding1.in_expression)
    }
}

impl<'src> VariableBinding<'src, NamesResolved> {
    fn link(&self, ctx: &mut Context<'src>) -> Result<VariableBinding<'src, Inference>> {
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

impl<'src> Expression<'src, NamesResolved> {
    fn link(&self, ctx: &mut Context<'src>) -> Result<ExpressionRef<'src>> {
        Ok(ExpressionRef::new(match self {
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

impl<'src> Expression<'src, Inference> {
    fn infer_types(&mut self) -> Result<()> {
        match self {
            Self::TypeOfType => (),
            Self::Constant { typ, .. } => typ.infer_types()?,
            Self::Variable(var) => {
                let value = &mut var.value;
                ExpressionRef::unify(&mut value.typ().typ(), &mut ExpressionRef::type_of_type())?;
                value.infer_types()?
            }
            Self::Apply {
                function,
                argument,
                typ,
            } => {
                ExpressionRef::unify(&mut typ.typ(), &mut ExpressionRef::type_of_type())?;
                let function_type =
                    &mut ExpressionRef::function_type(argument.clone(), typ.clone());
                ExpressionRef::unify(function_type, &mut function.typ())?;

                function.infer_types()?;
                argument.infer_types()?;
                typ.infer_types()?;
            }
            Self::Let(binding) => binding.infer_types()?,
            Self::FunctionType(binding) => binding.infer_types()?,
            Self::Lambda(binding) => binding.infer_types()?,
        }

        Ok(())
    }

    fn typ(&self) -> ExpressionRef<'src> {
        match self {
            Self::TypeOfType => ExpressionRef::type_of_type(),
            Self::Constant { typ, .. } => typ.clone(),
            Self::Variable(var) => var.value.typ(),
            Self::Apply { typ, .. } => typ.clone(),
            Self::Let(binding) => binding.in_expression.typ(),
            Self::FunctionType(_binding) => ExpressionRef::type_of_type(),
            Self::Lambda(binding) => ExpressionRef::function_type(
                binding.variable_value.clone(),
                binding.in_expression.typ(),
            ),
        }
    }
}

pub struct VariableReference<'src> {
    pub name: &'src str,
    pub value: ExpressionRef<'src>,
}

impl VariableReference<'_> {
    pub fn name(&self) -> &str {
        self.name
    }
}

impl fmt::Display for VariableReference<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{context::Context, source::SourceExpression as Expr, Pretty};

    #[test]
    fn infer_kinds() {
        // C : (m a)
        let m = Expr::variable("m");
        let a = Expr::variable("a");
        let ctx = &mut Context::new(HashMap::new());
        let mut constructor_type = Expr::lambda(
            "m",
            Expr::unknown_type(),
            Expr::lambda("a", Expr::unknown_type(), Expr::apply(m, a)),
        )
        .link(ctx)
        .unwrap();

        constructor_type.infer_types().unwrap();
        assert_eq!(
            constructor_type.to_pretty_string(80),
            r"\m : _ → _ ⇒ \a ⇒ (m : _ → _) a : _"
        );
    }

    #[test]
    fn let_error() {
        // let x : Int = 1 in x : Float
        let int_type = Expr::type_constant("Int");
        let float_type = Expr::type_constant("Float");
        let one = Expr::variable("one").has_type(int_type.clone());
        let let_binding = Expr::let_binding("x", one, Expr::variable("x").has_type(float_type));

        let mut global_types = HashMap::new();
        global_types.insert("one", int_type.resolve_names().unwrap());
        global_types.insert("Int", Expr::type_of_type().resolve_names().unwrap());
        global_types.insert("Float", Expr::type_of_type().resolve_names().unwrap());
        let ctx = &mut Context::new(global_types);

        assert!(let_binding.link(ctx).is_err());
    }
}
