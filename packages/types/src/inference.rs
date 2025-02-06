use std::{
    cell::{RefCell, RefMut},
    collections::BTreeSet,
    fmt,
    ops::ControlFlow,
    rc::Rc,
};

use crate::{
    ExpressionReference, GenericExpression, InferenceError, Pretty, Result, SharedMut,
    VariableBinding,
};

mod pretty;

#[derive(Clone)]
pub struct Expression<'src>(SharedMut<ExprVariants<'src>>);

#[derive(Default)]
struct Names<'src>(BTreeSet<&'src str>);

impl<'src> Names<'src> {
    fn add(&mut self, name: &'src str) {
        self.0.insert(name);
    }

    fn merge(&mut self, other: &Self) {
        self.0.extend(&other.0);
    }
}

enum ExprVariants<'src> {
    Known {
        names: Names<'src>,
        expression: GenericExpression<'src, Expression<'src>>,
    },
    Unknown {
        names: Names<'src>,
        typ: Expression<'src>,
    },
    Link {
        target: Expression<'src>,
    },
}

impl<'src> ExprVariants<'src> {
    fn add_name(&mut self, name: &'src str) {
        match self {
            ExprVariants::Known { names, .. } => names.add(name),
            ExprVariants::Unknown { names, .. } => names.add(name),
            ExprVariants::Link { target } => target.0.borrow_mut().add_name(name),
        }
    }
}

impl<'src> Expression<'src> {
    pub fn unknown(typ: Self) -> Self {
        Self(Rc::new(RefCell::new(ExprVariants::Unknown {
            names: Names::default(),
            typ,
        })))
    }

    pub fn unknown_value() -> Self {
        Self::unknown(Self::unknown_type())
    }

    pub fn unknown_type() -> Self {
        Self::unknown(Self::type_of_type())
    }

    fn type_of_type() -> Self {
        Self::new(GenericExpression::TypeOfType)
    }

    fn function_type(argument_value: Self, result_type: Self) -> Self {
        Self::new(GenericExpression::Pi(VariableBinding {
            name: "_",
            variable_value: argument_value,
            in_expression: result_type,
        }))
    }

    pub fn variable(name: &'src str, value: Self) -> Self {
        value.0.borrow_mut().add_name(name);
        value
    }

    pub(crate) fn new(expression: GenericExpression<'src, Self>) -> Self {
        Self(Rc::new(RefCell::new(ExprVariants::Known {
            names: Names::default(),
            expression,
        })))
    }

    pub fn infer_types(&mut self) -> Result<()> {
        Self::unify(&mut self.typ().typ(), &mut Self::type_of_type())?;

        match &mut *self.0.borrow_mut() {
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
            let ExprVariants::Unknown { typ, .. } = &mut *self.0.borrow_mut() else {
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

        if Rc::ptr_eq(&x.0, &y.0)
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
            (GenericExpression::Let(binding0), GenericExpression::Let(binding1)) => {
                VariableBinding::unify(binding0, binding1)?
            }
            (GenericExpression::Pi(binding0), GenericExpression::Pi(binding1)) => {
                VariableBinding::unify(binding0, binding1)?
            }
            (GenericExpression::Lambda(binding0), GenericExpression::Lambda(binding1)) => {
                VariableBinding::unify(binding0, binding1)?
            }
            // It's safer to explicitly ignore each variant
            (GenericExpression::TypeOfType, _rhs)
            | (GenericExpression::Constant { .. }, _rhs)
            | (GenericExpression::Apply { .. }, _rhs)
            | (GenericExpression::Let { .. }, _rhs)
            | (GenericExpression::Pi(_), _rhs)
            | (GenericExpression::Lambda(_), _rhs) => Err(InferenceError)?,
        }

        drop(x_ref);
        drop(y_ref);
        x.replace_with(y);

        Ok(())
    }

    fn replace_with(&mut self, other: &Self) {
        if let (Some(mut other_names), Some(names)) = (other.names(), self.names()) {
            other_names.merge(&names);
        }

        RefCell::replace(
            &self.0,
            ExprVariants::Link {
                target: other.clone(),
            },
        );
        *self = other.clone();
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

    fn names<'a>(&'a self) -> Option<RefMut<'a, Names<'src>>> {
        RefMut::filter_map(self.0.borrow_mut(), |x| match x {
            ExprVariants::Known { names, .. } | ExprVariants::Unknown { names, .. } => Some(names),
            ExprVariants::Link { .. } => None,
        })
        .ok()
    }
}

impl fmt::Debug for Expression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.to_pretty_string(80))
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
            ExprVariants::Known { expression, .. } => expression.typ(Self::new),
            ExprVariants::Unknown { typ, .. } => typ.clone(),
            ExprVariants::Link { target } => target.typ(),
        }
    }
}

impl<'src> VariableBinding<'src, Expression<'src>> {
    fn infer_types(&mut self) -> Result<()> {
        self.variable_value.infer_types()?;
        self.in_expression.infer_types()
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
                Expression::unify(&mut typ.typ(), &mut Expression::type_of_type())?;
                let function_type = &mut Expression::function_type(argument.clone(), typ.clone());
                Expression::unify(function_type, &mut function.typ())?;

                function.infer_types()?;
                argument.infer_types()?;
                typ.infer_types()?;
            }
            Self::Let(binding) => binding.infer_types()?,
            Self::Pi(binding) => binding.infer_types()?,
            Self::Lambda(binding) => binding.infer_types()?,
        }

        Ok(())
    }
}
