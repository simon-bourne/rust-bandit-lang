use std::{
    cell::{RefCell, RefMut},
    fmt,
    ops::ControlFlow,
    rc::Rc,
};

use crate::{Expression, InferenceError, Pretty, Result, SharedMut, Stage, VariableBinding};

mod pretty;

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
    pub fn unknown(typ: Self) -> Self {
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

    pub fn variable(name: &'src str, value: Self) -> Self {
        Self::new(Expression::Variable(VariableReference { name, value }))
    }

    // TODO: Make private
    pub(crate) fn new(expression: Expression<'src, Inference>) -> Self {
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
        // Collapse links from the bottom up so they are also collapsed for other
        // expressions that reference this chain.
        let link = {
            let ExprRefVariants::Link { target } = &mut *self.0.borrow_mut() else {
                return;
            };
            target.follow_links();
            target.clone()
        };

        *self = link;
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

    pub fn unify(x: &mut Self, y: &mut Self) -> Result<()> {
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

        // TODO: Application evaluation:
        //
        // - If all types are known, and the expression is `Reducable::Maybe`:
        //      - Evaluate the expression as much as possible.
        // - Otherwise add to a list of deferred unifications.
        // - Only unify applications if they're both `Reducable::No` (meaning we've
        //   tried to evaluate the expression).

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

    pub fn typ(&self) -> Self {
        match &*self.0.borrow() {
            ExprRefVariants::Known { expression } => expression.typ(),
            ExprRefVariants::Unknown { typ } => typ.clone(),
            ExprRefVariants::Link { target } => target.typ(),
        }
    }
}

impl fmt::Debug for ExpressionRef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.to_pretty_string(80))
    }
}

pub(crate) struct Inference;

impl<'src> Stage<'src> for Inference {
    type Expression = ExpressionRef<'src>;
    type Variable = VariableReference<'src>;
}

pub(crate) struct VariableReference<'src> {
    name: &'src str,
    value: ExpressionRef<'src>,
}

impl fmt::Debug for VariableReference<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.to_pretty_string(80))
    }
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
