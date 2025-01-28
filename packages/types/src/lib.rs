use std::{
    cell::{RefCell, RefMut},
    rc::Rc,
    result,
};

use context::{Context, VariableReference};

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

// TODO: Rename to Linked?
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
        Self::new(Expression::Type)
    }

    fn function_type(argument_type: Self, result_type: Self) -> Self {
        Self::new(Expression::FunctionType(VariableBinding {
            name: "_",
            variable_type: argument_type,
            in_expression: result_type,
        }))
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

    fn unify(x: &mut Self, y: &mut Self) -> Result<()> {
        x.follow_links();
        y.follow_links();

        if Rc::ptr_eq(&x.0, &y.0) {
            return Ok(());
        }

        let Some(mut x_ref) = x.known() else {
            Self::unify(&mut x.typ(), &mut y.typ())?;
            x.replace(y);
            return Ok(());
        };

        if let Expression::Variable(var) = &mut *x_ref {
            return Self::unify(&mut var.typ, &mut y.typ());
        }

        let Some(mut y_ref) = y.known() else {
            drop(x_ref);
            Self::unify(&mut x.typ(), &mut y.typ())?;
            y.replace(x);
            return Ok(());
        };

        if let Expression::Variable(var) = &mut *y_ref {
            drop(x_ref);
            return Self::unify(&mut var.typ, &mut x.typ());
        }

        // TODO: Can we use mutable borrowing to do the occurs check for us?
        match (&mut *x_ref, &mut *y_ref) {
            (Expression::Type, Expression::Type) => {}
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
            (
                Expression::Let {
                    variable_value,
                    binding,
                },
                Expression::Let {
                    variable_value: variable_value1,
                    binding: binding1,
                },
            ) => {
                Self::unify(variable_value, variable_value1)?;
                VariableBinding::unify(binding, binding1)?;
            }
            (Expression::FunctionType(binding0), Expression::FunctionType(binding1)) => {
                VariableBinding::unify(binding0, binding1)?
            }
            (Expression::Lambda(binding0), Expression::Lambda(binding1)) => {
                VariableBinding::unify(binding0, binding1)?
            }
            // It's safer to explicitly ignore each variant
            (Expression::Type, _rhs)
            | (Expression::Apply { .. }, _rhs)
            | (Expression::Let { .. }, _rhs)
            | (Expression::FunctionType(_), _rhs)
            | (Expression::Lambda(_), _rhs)
            | (Expression::Variable(_), _rhs) => Err(InferenceError)?,
        }

        drop(x_ref);
        drop(y_ref);
        x.replace(y);

        Ok(())
    }

    fn replace(&mut self, other: &Self) {
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
    Type,
    Apply {
        function: S::Expression,
        argument: S::Expression,
        typ: S::Expression,
    },
    Let {
        variable_value: S::Expression,
        binding: VariableBinding<'src, S>,
    },
    // TODO: Do we need to store a constraint on the binding, like `a ~ expression`?
    FunctionType(VariableBinding<'src, S>),
    Lambda(VariableBinding<'src, S>),
    Variable(S::Variable),
}

struct VariableBinding<'src, S: Stage<'src>> {
    name: &'src str,
    variable_type: S::Expression,
    in_expression: S::Expression,
}

impl VariableBinding<'_, Inference> {
    fn infer_types(&mut self) -> Result<()> {
        ExpressionRef::unify(
            &mut self.variable_type.typ(),
            &mut ExpressionRef::type_of_type(),
        )?;
        self.variable_type.infer_types()?;
        self.in_expression.infer_types()
    }

    fn unify(binding0: &mut Self, binding1: &mut Self) -> Result<()> {
        ExpressionRef::unify(&mut binding0.variable_type, &mut binding1.variable_type)?;
        ExpressionRef::unify(&mut binding0.in_expression, &mut binding1.in_expression)
    }
}

impl<'src> VariableBinding<'src, NamesResolved> {
    fn link(&self, ctx: &mut Context<'src>) -> Result<VariableBinding<'src, Inference>> {
        let name = self.name;
        let variable_type = self.variable_type.link(ctx)?;
        let in_expression =
            ctx.with_variable(variable_type.clone(), |ctx| self.in_expression.link(ctx))?;

        Ok(VariableBinding {
            name,
            variable_type,
            in_expression,
        })
    }
}

impl<'src> Expression<'src, NamesResolved> {
    fn link(&self, ctx: &mut Context<'src>) -> Result<ExpressionRef<'src>> {
        Ok(ExpressionRef::new(match self {
            Self::Type => Expression::Type,
            Self::Apply {
                function,
                argument,
                typ,
            } => Expression::Apply {
                function: function.link(ctx)?,
                argument: argument.link(ctx)?,
                typ: typ.link(ctx)?,
            },
            Self::Let {
                variable_value,
                binding,
            } => Expression::Let {
                variable_value: variable_value.link(ctx)?,
                binding: binding.link(ctx)?,
            },
            Self::FunctionType(binding) => Expression::FunctionType(binding.link(ctx)?),
            Self::Lambda(binding) => Expression::Lambda(binding.link(ctx)?),
            Self::Variable(variable) => Expression::Variable(ctx.lookup_type(*variable)?),
        }))
    }
}

impl<'src> Expression<'src, Inference> {
    fn infer_types(&mut self) -> Result<()> {
        match self {
            Self::Type => (),
            Self::Variable(var) => {
                let typ = &mut var.typ;
                ExpressionRef::unify(&mut typ.typ(), &mut ExpressionRef::type_of_type())?;
                typ.infer_types()?
            }
            Self::Apply {
                function,
                argument,
                typ,
            } => {
                ExpressionRef::unify(&mut typ.typ(), &mut ExpressionRef::type_of_type())?;
                let function_type = &mut ExpressionRef::function_type(argument.typ(), typ.clone());
                ExpressionRef::unify(function_type, &mut function.typ())?;

                function.infer_types()?;
                argument.infer_types()?;
                typ.infer_types()?;
            }
            Self::Let {
                variable_value,
                binding,
            } => {
                variable_value.infer_types()?;
                binding.infer_types()?;
            }
            Self::FunctionType(binding) => binding.infer_types()?,
            Self::Lambda(binding) => binding.infer_types()?,
        }

        Ok(())
    }

    fn typ(&self) -> ExpressionRef<'src> {
        match self {
            Self::Type => ExpressionRef::type_of_type(),
            Self::Variable(var) => var.typ.clone(),
            Self::Apply { typ, .. } => typ.clone(),
            Self::Let { binding, .. } => binding.in_expression.typ(),
            Self::FunctionType(_binding) => ExpressionRef::type_of_type(),
            Self::Lambda(binding) => ExpressionRef::function_type(
                binding.variable_type.clone(),
                binding.in_expression.typ(),
            ),
        }
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
            r"\m : _ → _ ⇒ \a ⇒ (m : _ → _) a"
        );
    }

    #[test]
    // TODO: This test should pass. It currently tries to unify the variables "Int" and "Float".
    // These are both types and so have a type of "Type". Because we only try and unify the
    // variable's types, this succeeds.
    #[should_panic]
    fn let_error() {
        // let x : Int = 1 in x : Float
        let int_type = Expr::variable("Int");
        let float_type = Expr::variable("Float");
        let one = Expr::variable("one").has_type(int_type.clone());
        let let_binding = Expr::let_binding(
            "x",
            int_type.clone(),
            one,
            Expr::variable("x").has_type(float_type),
        );

        let mut global_types = HashMap::new();
        global_types.insert("one", int_type.resolve_names().unwrap());
        global_types.insert("Int", Expr::type_of_type().resolve_names().unwrap());
        global_types.insert("Float", Expr::type_of_type().resolve_names().unwrap());
        let ctx = &mut Context::new(global_types);

        assert!(let_binding.link(ctx).unwrap().infer_types().is_err());
    }
}
