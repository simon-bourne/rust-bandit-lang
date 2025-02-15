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
        pass: u64,
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
        if let Some(new_variable) = new_variables.get(&self.0.as_ptr()) {
            return new_variable.clone();
        }

        match &*self.0.borrow() {
            ExprVariants::Known {
                pass,
                name,
                expression,
            } => {
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
                    pass: *pass,
                    name: *name,
                    expression: generic_expression,
                })
            }
            ExprVariants::Unknown { .. } => self.clone(),
            ExprVariants::Link { target } => target.make_fresh_variables(new_variables),
        }
    }

    fn copy_if_unknown(&self, new_variables: &mut OldToNewVariable<'src>) -> Self {
        let key = self.0.as_ptr();

        if let Some(variable) = new_variables.get(&key) {
            return variable.clone();
        }

        match &*self.0.borrow() {
            ExprVariants::Known { .. } => self.clone(),
            ExprVariants::Unknown { name, typ } => {
                let new_unknown = Self::new(ExprVariants::Unknown {
                    name: *name,
                    typ: typ.make_fresh_variables(new_variables),
                });
                new_variables.insert(key, new_unknown.clone());

                new_unknown
            }
            ExprVariants::Link { target } => target.copy_if_unknown(new_variables),
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

    fn type_of_type(pass: u64) -> Self {
        Self::new_known(pass, GenericExpression::TypeOfType)
    }

    fn pi_type(pass: u64, argument_value: Self, result_type: Self) -> Self {
        Self::new_known(
            pass,
            GenericExpression::pi(None, argument_value, result_type),
        )
    }

    pub(crate) fn new_known(pass: u64, expression: GenericExpression<'src, Self>) -> Self {
        Self(SharedMut::new(ExprVariants::Known {
            pass,
            name: None,
            expression,
        }))
    }

    fn new(expression: ExprVariants<'src>) -> Self {
        Self(SharedMut::new(expression))
    }

    pub fn infer_types(&mut self, current_pass: u64) -> Result<()> {
        Self::unify(
            &mut self.typ().typ().fresh_variables(),
            &mut Self::type_of_type(current_pass),
        )?;

        match &mut *self.0.try_borrow_mut()? {
            ExprVariants::Known {
                pass, expression, ..
            } => {
                if current_pass <= *pass {
                    *pass = current_pass + 1;
                    expression.infer_types(current_pass)?
                }
            }
            ExprVariants::Unknown { typ, .. } => typ.infer_types(current_pass)?,
            ExprVariants::Link { target } => target.infer_types(current_pass)?,
        }

        Ok(())
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
        let mut typ = {
            let ExprVariants::Unknown { typ, .. } = &mut *self.0.try_borrow_mut()? else {
                return Ok(ControlFlow::Continue(()));
            };
            typ.clone()
        };

        // We need to replace ourself with `other` in case `typ` references `self`.
        self.replace_with(other);
        Self::unify(&mut typ, &mut other.typ())?;

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
            ExprVariants::Known {
                pass, expression, ..
            } => expression.typ(|e| Self::new_known(*pass, e)),
            ExprVariants::Unknown { typ, .. } => typ.clone(),
            ExprVariants::Link { target } => target.typ(),
        }
    }
}

impl<'src> VariableBinding<'src, Expression<'src>> {
    fn infer_types(&mut self, pass: u64) -> Result<()> {
        self.variable_value.infer_types(pass)?;
        self.in_expression.infer_types(pass)
    }

    fn fresh_variables(&self, new_variables: &mut OldToNewVariable<'src>) -> Self {
        let variable_value = self.variable_value.copy_if_unknown(new_variables);
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
    fn infer_types(&mut self, pass: u64) -> Result<()> {
        match self {
            Self::TypeOfType => (),
            Self::Constant { typ, .. } => typ.infer_types(pass)?,
            Self::Apply {
                function,
                argument,
                typ,
            } => {
                Expression::unify(
                    &mut typ.typ().fresh_variables(),
                    &mut Expression::type_of_type(pass),
                )?;
                // TODO: Is this reasoning sound?
                // We're creating a new bound variable (`argument`) here. If it's unknown, we
                // want to infer it, so we don't want it to be fresh. Therefore we create fresh
                // variables for `argument` and `typ`, not `pi_type`.
                let pi_type = &mut Expression::pi_type(
                    pass,
                    argument.fresh_variables(),
                    typ.fresh_variables(),
                );
                Expression::unify(pi_type, &mut function.typ().fresh_variables())?;

                function.infer_types(pass)?;
                argument.infer_types(pass)?;
                typ.infer_types(pass)?;
            }
            Self::VariableBinding(binding) => binding.infer_types(pass)?,
        }

        Ok(())
    }
}
