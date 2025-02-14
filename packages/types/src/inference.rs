use std::{cell::RefMut, collections::HashMap, fmt, ops::ControlFlow};

use crate::{
    ExpressionReference, GenericExpression, InferenceError, Pretty, Result, SharedMut,
    VariableBinding,
};

mod pretty;

#[derive(Clone)]
pub struct Expression<'src>(SharedMut<ExprVariants<'src>>);

enum ExprVariants<'src> {
    Known {
        name: Option<&'src str>,
        expression: GenericExpression<'src, Expression<'src>>,
    },
    Unknown {
        name: Option<&'src str>,
        typ: Expression<'src>,
    },
    Link {
        target: Expression<'src>,
    },
}

type OldToNewVariable<'src> = HashMap<*mut ExprVariants<'src>, Expression<'src>>;

impl<'src> Expression<'src> {
    pub fn unknown(typ: Self) -> Self {
        Self::new(ExprVariants::Unknown { name: None, typ })
    }

    // TODO: The implementation of this is ugly and inefficient.
    pub fn fresh_variables(&self) -> Self {
        self.make_fresh_variables(&mut HashMap::new())
    }

    fn make_fresh_variables(&self, new_variables: &mut OldToNewVariable<'src>) -> Self {
        if let Some(bound_variable) = new_variables.get(&self.0.as_ptr()) {
            return bound_variable.clone();
        }

        match &*self.0.borrow() {
            ExprVariants::Known { name, expression } => {
                let generic_expression = match expression {
                    GenericExpression::VariableBinding(binding) => {
                        GenericExpression::VariableBinding(binding.fresh_variables(new_variables))
                    }
                    GenericExpression::TypeOfType => GenericExpression::TypeOfType,
                    GenericExpression::Constant { name, typ } => GenericExpression::Constant {
                        name,
                        typ: typ.make_fresh_variables(new_variables),
                    },
                    GenericExpression::Apply {
                        function,
                        argument,
                        typ,
                    } => GenericExpression::Apply {
                        function: function.make_fresh_variables(new_variables),
                        argument: argument.make_fresh_variables(new_variables),
                        typ: typ.make_fresh_variables(new_variables),
                    },
                };

                Self::new(ExprVariants::Known {
                    name: *name,
                    expression: generic_expression,
                })
            }
            ExprVariants::Unknown { .. } => self.clone(),
            ExprVariants::Link { target } => target.make_fresh_variables(new_variables),
        }
    }

    fn deep_copy(&self, new_variables: &mut OldToNewVariable<'src>) -> Self {
        let key = self.0.as_ptr();

        if let Some(variable) = new_variables.get(&key) {
            return variable.clone();
        }

        match &*self.0.borrow() {
            ExprVariants::Known { name, expression } => Self::new(ExprVariants::Known {
                name: *name,
                expression: expression.map_expression(|expr| expr.deep_copy(new_variables)),
            }),
            ExprVariants::Unknown { name, typ } => {
                let new_unknown = Self::new(ExprVariants::Unknown {
                    name: *name,
                    typ: typ.deep_copy(new_variables),
                });
                new_variables.insert(key, new_unknown.clone());

                new_unknown
            }
            ExprVariants::Link { target } => target.deep_copy(new_variables),
        }
    }

    fn name(&self) -> Option<&'src str> {
        match &*self.0.borrow() {
            ExprVariants::Known { name, .. } | ExprVariants::Unknown { name, .. } => *name,
            ExprVariants::Link { target } => target.name(),
        }
    }

    pub fn set_name(&mut self, new_name: &'src str) {
        match &mut *self.0.borrow_mut() {
            ExprVariants::Known { name, .. } | ExprVariants::Unknown { name, .. } => {
                *name = Some(new_name)
            }
            ExprVariants::Link { target } => target.set_name(new_name),
        }
    }

    fn type_of_type() -> Self {
        Self::new_known(GenericExpression::TypeOfType)
    }

    fn pi_type(argument_value: Self, result_type: Self) -> Self {
        Self::new_known(GenericExpression::pi(None, argument_value, result_type))
    }

    pub(crate) fn new_known(expression: GenericExpression<'src, Self>) -> Self {
        Self(SharedMut::new(ExprVariants::Known {
            name: None,
            expression,
        }))
    }

    fn new(expression: ExprVariants<'src>) -> Self {
        Self(SharedMut::new(expression))
    }

    /// Infer types, unless `self` is a bound variable, in which case do
    /// nothing.
    pub fn infer_types(&mut self) -> Result<()> {
        // We want to return early if this is a bound name, unless this is the binding
        // site. For example, in `let x = y in x`, the second `x` will just be a
        // reference to `y` and we want to return early.
        if self.name().is_some() {
            Ok(())
        } else {
            self.always_infer_types()
        }
    }

    fn always_infer_types(&mut self) -> Result<()> {
        Self::unify(
            &mut self.typ().typ().fresh_variables(),
            &mut Self::type_of_type(),
        )?;

        match &mut *self.0.try_borrow_mut()? {
            ExprVariants::Known { expression, .. } => expression.infer_types(),
            ExprVariants::Unknown { typ, .. } => typ.infer_types(),
            ExprVariants::Link { target } => target.infer_types(),
        }
    }

    fn collapse_links(&mut self) {
        // Collapse links from the bottom up so they are also collapsed for other
        // expressions that reference this chain.
        *self = {
            let ExprVariants::Link { target } = &mut *self.0.borrow_mut() else {
                return;
            };
            target.collapse_links();
            target.clone()
        };
    }

    fn unify_unknown(&mut self, other: &mut Self) -> Result<ControlFlow<()>> {
        {
            let ExprVariants::Unknown { typ, .. } = &mut *self.0.try_borrow_mut()? else {
                return Ok(ControlFlow::Continue(()));
            };

            Self::unify(typ, &mut other.typ())?;
        }

        self.replace_with(other);

        Ok(ControlFlow::Break(()))
    }

    pub fn unify(x: &mut Self, y: &mut Self) -> Result<()> {
        x.collapse_links();
        y.collapse_links();

        if SharedMut::is_same(&x.0, &y.0)
            || x.unify_unknown(y)?.is_break()
            || y.unify_unknown(x)?.is_break()
        {
            return Ok(());
        }

        let mut x_ref = x.known().expect("x should be known at this point");
        let mut y_ref = y.known().expect("y should be known at this point");

        // TODO: Application evaluation:
        //
        // - If all types are known, and the expression is `Reducible::Maybe`:
        //      - Evaluate the expression as much as possible.
        // - Otherwise add to a list of deferred unifications.
        // - Only unify applications if they're both `Reducible::No` (meaning we've
        //   tried to evaluate the expression).
        // - Reducible has another variant of `Reducible::PendingTypeCheck`

        // TODO: Can we use mutable borrowing to do the occurs check for us?
        match (&mut *x_ref, &mut *y_ref) {
            (GenericExpression::TypeOfType, GenericExpression::TypeOfType) => (),
            (
                GenericExpression::Constant {
                    name: name0,
                    typ: typ0,
                },
                GenericExpression::Constant {
                    name: name1,
                    typ: typ1,
                },
            ) if name0 == name1 => Self::unify(typ0, typ1)?,
            (
                GenericExpression::Apply {
                    function,
                    argument,
                    typ,
                },
                GenericExpression::Apply {
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
                GenericExpression::VariableBinding(binding0),
                GenericExpression::VariableBinding(binding1),
            ) => VariableBinding::unify(binding0, binding1)?,
            // It's safer to explicitly ignore each variant
            (GenericExpression::TypeOfType, _rhs)
            | (GenericExpression::Constant { .. }, _rhs)
            | (GenericExpression::Apply { .. }, _rhs)
            | (GenericExpression::VariableBinding(_), _rhs) => Err(InferenceError)?,
        }

        drop(x_ref);
        drop(y_ref);
        x.replace_with(y);

        Ok(())
    }

    fn replace_with(&mut self, other: &Self) {
        let name = other.name().or_else(|| self.name());

        self.0.replace_with(ExprVariants::Link {
            target: other.clone(),
        });
        *self = other.clone();

        if let Some(name) = name {
            self.set_name(name);
        }
    }

    fn known<'a>(&'a self) -> Option<RefMut<'a, GenericExpression<'src, Self>>> {
        RefMut::filter_map(self.0.borrow_mut(), |x| {
            if let ExprVariants::Known { expression, .. } = x {
                Some(expression)
            } else {
                None
            }
        })
        .ok()
    }
}

impl fmt::Debug for Expression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.debug())
    }
}

impl<'src> ExpressionReference<'src> for Expression<'src> {
    fn is_known(&self) -> bool {
        match &*self.0.borrow() {
            ExprVariants::Known { .. } => true,
            ExprVariants::Unknown { .. } => false,
            ExprVariants::Link { target } => target.is_known(),
        }
    }

    fn typ(&self) -> Self {
        match &*self.0.borrow() {
            ExprVariants::Known { expression, .. } => expression.typ(Self::new_known),
            ExprVariants::Unknown { typ, .. } => typ.clone(),
            ExprVariants::Link { target } => target.typ(),
        }
    }
}

impl<'src> VariableBinding<'src, Expression<'src>> {
    fn infer_types(&mut self) -> Result<()> {
        self.variable_value.always_infer_types()?;
        self.in_expression.infer_types()
    }

    fn fresh_variables(&self, new_variables: &mut OldToNewVariable<'src>) -> Self {
        let variable_value = self.variable_value.deep_copy(new_variables);
        let in_expression = self.in_expression.make_fresh_variables(new_variables);

        Self {
            name: self.name,
            binder: self.binder,
            variable_value,
            in_expression,
        }
    }

    fn unify(binding0: &mut Self, binding1: &mut Self) -> Result<()> {
        Expression::unify(&mut binding0.variable_value, &mut binding1.variable_value)?;
        Expression::unify(&mut binding0.in_expression, &mut binding1.in_expression)
    }
}

impl<'src> GenericExpression<'src, Expression<'src>> {
    fn infer_types(&mut self) -> Result<()> {
        match self {
            Self::TypeOfType => (),
            Self::Constant { typ, .. } => typ.infer_types()?,
            Self::Apply {
                function,
                argument,
                typ,
            } => {
                Expression::unify(
                    &mut typ.typ().fresh_variables(),
                    &mut Expression::type_of_type(),
                )?;
                // TODO: Is this reasoning sound?
                // We're creating a new bound variable (`argument`) here. If it's unknown, we
                // want to infer it, so we don't want it to be fresh. Therefore we create fresh
                // variables for `argument` and `typ`, not `pi_type`.
                let pi_type =
                    &mut Expression::pi_type(argument.fresh_variables(), typ.fresh_variables());
                Expression::unify(pi_type, &mut function.typ().fresh_variables())?;

                function.infer_types()?;
                argument.infer_types()?;
                typ.infer_types()?;
            }
            Self::VariableBinding(binding) => binding.infer_types()?,
        }

        Ok(())
    }
}
