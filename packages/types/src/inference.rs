use std::{cell::RefMut, collections::HashMap, fmt, ops::ControlFlow};

use crate::{
    GenericTerm, InferenceError, Pretty, Result, SharedMut, TermReference, VariableBinding,
};

mod pretty;

#[derive(Clone)]
pub struct Term<'src>(SharedMut<TermVariants<'src>>);

enum TermVariants<'src> {
    Value {
        pass: u64,
        term: GenericTerm<'src, Term<'src>>,
    },
    Link {
        target: Term<'src>,
    },
}

type OldToNewVariable<'src> = HashMap<*mut TermVariants<'src>, Term<'src>>;

impl<'src> Term<'src> {
    pub fn unknown(typ: Self) -> Self {
        Self::new(0, GenericTerm::Variable(Variable { name: None, typ }))
    }

    pub fn unknown_value() -> Self {
        Self::unknown(Self::unknown_type())
    }

    pub fn unknown_type() -> Self {
        Self::unknown(Self::type_of_type(0))
    }

    // TODO: The implementation of this is ugly and inefficient.
    pub fn set_variable_name(&mut self, name: &'src str) {
        if let GenericTerm::Variable(variable) = &mut *self.value() {
            variable.name = Some(name);
        }
    }

    pub fn fresh_variables(&mut self) -> Self {
        self.make_fresh_variables(&mut HashMap::new())
    }

    fn make_fresh_variables(&mut self, new_variables: &mut OldToNewVariable<'src>) -> Self {
        if let Some(new_variable) = new_variables.get(&self.0.as_ptr()) {
            return new_variable.clone();
        }

        let mut value = self.value();

        let generic_term = match &mut *value {
            GenericTerm::Variable(_) => {
                drop(value);
                return self.clone();
            }
            GenericTerm::VariableBinding(binding) => {
                GenericTerm::VariableBinding(binding.make_fresh_variables(new_variables))
            }
            GenericTerm::TypeOfType => GenericTerm::TypeOfType,
            GenericTerm::Constant { name, typ } => GenericTerm::Constant {
                name,
                typ: typ.make_fresh_variables(new_variables),
            },
            GenericTerm::Apply {
                function,
                argument,
                typ,
            } => GenericTerm::Apply {
                function: function.make_fresh_variables(new_variables),
                argument: argument.make_fresh_variables(new_variables),
                typ: typ.make_fresh_variables(new_variables),
            },
        };

        Self::new(0, generic_term)
    }

    fn type_of_type(pass: u64) -> Self {
        Self::new(pass, GenericTerm::TypeOfType)
    }

    fn pi_type(pass: u64, argument_value: Self, result_type: Self) -> Self {
        Self::new(pass, GenericTerm::pi(None, argument_value, result_type))
    }

    pub(crate) fn new(pass: u64, term: GenericTerm<'src, Self>) -> Self {
        Self(SharedMut::new(TermVariants::Value { pass, term }))
    }

    pub fn infer_types(&mut self, current_pass: u64) -> Result<()> {
        Self::unify(
            &mut self.typ().typ().fresh_variables(),
            &mut Self::type_of_type(current_pass),
        )?;

        match &mut *self.0.try_borrow_mut()? {
            TermVariants::Value { pass, term } => {
                if current_pass <= *pass {
                    *pass = current_pass + 1;
                    term.infer_types(current_pass)?
                }
            }
            TermVariants::Link { target } => target.infer_types(current_pass)?,
        }

        Ok(())
    }

    fn unify_bound_variable(&mut self, other: &mut Self) -> Result<ControlFlow<()>> {
        let mut self_ref = self.value();
        let mut other_ref = other.value();

        Ok(match (&mut *self_ref, &mut *other_ref) {
            (
                GenericTerm::Variable(Variable { name: Some(_), typ }),
                GenericTerm::Variable(Variable {
                    name: None,
                    typ: other_typ,
                }),
            ) => {
                let mut other_type = other_typ.clone();
                let mut typ = typ.clone();
                drop((self_ref, other_ref));
                other.replace_with(self);
                Self::unify(&mut typ, &mut other_type)?;

                ControlFlow::Break(())
            }
            (GenericTerm::Variable(Variable { name: Some(_), typ }), _) => {
                // TODO: Prefer variables with smaller scope
                let mut typ = typ.clone();
                drop((self_ref, other_ref));
                self.replace_with(other);
                Self::unify(&mut typ, &mut other.typ())?;

                ControlFlow::Break(())
            }
            _ => ControlFlow::Continue(()),
        })
    }

    fn unify_variable(&mut self, other: &mut Self) -> Result<ControlFlow<()>> {
        let mut self_ref = self.value();

        Ok(
            if let GenericTerm::Variable(Variable { typ, .. }) = &mut *self_ref {
                let mut typ = typ.clone();
                drop(self_ref);
                self.replace_with(other);
                Self::unify(&mut typ, &mut other.typ())?;

                ControlFlow::Break(())
            } else {
                ControlFlow::Continue(())
            },
        )
    }

    pub fn unify(x: &mut Self, y: &mut Self) -> Result<()> {
        x.collapse_links();
        y.collapse_links();

        if SharedMut::is_same(&x.0, &y.0)
            || x.unify_bound_variable(y)?.is_break()
            || y.unify_bound_variable(x)?.is_break()
            || x.unify_variable(y)?.is_break()
            || y.unify_variable(x)?.is_break()
        {
            return Ok(());
        }

        let mut x_ref = x.value();
        let mut y_ref = y.value();

        // TODO: Application evaluation:
        //
        // - If all types are known, and the term is `Reducible::Maybe`:
        //      - Evaluate the term as much as possible.
        // - Otherwise add to a list of deferred unifications.
        // - Only unify applications if they're both `Reducible::No` (meaning we've
        //   tried to evaluate the term).
        // - Reducible has another variant of `Reducible::PendingTypeCheck`

        // TODO: Can we use mutable borrowing to do the occurs check for us?
        match (&mut *x_ref, &mut *y_ref) {
            (GenericTerm::TypeOfType, GenericTerm::TypeOfType) => (),
            (
                GenericTerm::Constant {
                    name: name0,
                    typ: typ0,
                },
                GenericTerm::Constant {
                    name: name1,
                    typ: typ1,
                },
            ) if name0 == name1 => Self::unify(typ0, typ1)?,
            (
                GenericTerm::Apply {
                    function,
                    argument,
                    typ,
                },
                GenericTerm::Apply {
                    function: function1,
                    argument: argument1,
                    typ: typ1,
                },
            ) => {
                Self::unify(function, function1)?;
                Self::unify(argument, argument1)?;
                Self::unify(typ, typ1)?;
            }
            (GenericTerm::VariableBinding(binding0), GenericTerm::VariableBinding(binding1)) => {
                VariableBinding::unify(binding0, binding1)?
            }
            // It's safer to explicitly ignore each variant
            (GenericTerm::TypeOfType, _rhs)
            | (GenericTerm::Constant { .. }, _rhs)
            | (GenericTerm::Apply { .. }, _rhs)
            | (GenericTerm::Variable(_), _rhs)
            | (GenericTerm::VariableBinding(_), _rhs) => Err(InferenceError)?,
        }

        drop(x_ref);
        drop(y_ref);
        x.replace_with(y);

        Ok(())
    }

    fn replace_with(&mut self, other: &Self) {
        self.0.replace_with(TermVariants::Link {
            target: other.clone(),
        });
        *self = other.clone();
    }

    fn collapse_links(&mut self) {
        // Collapse links from the bottom up so they are also collapsed for other
        // terms that reference this chain.
        *self = {
            let TermVariants::Link { target } = &mut *self.0.borrow_mut() else {
                return;
            };
            target.collapse_links();
            target.clone()
        };
    }

    fn value<'a>(&'a mut self) -> RefMut<'a, GenericTerm<'src, Self>> {
        self.collapse_links();
        RefMut::map(self.0.borrow_mut(), |x| match x {
            TermVariants::Value { term, .. } => term,
            TermVariants::Link { .. } => unreachable!("Links should be collapsed at this point"),
        })
    }
}

impl fmt::Debug for Term<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.debug())
    }
}

pub struct Variable<'src> {
    name: Option<&'src str>,
    typ: Term<'src>,
}

impl<'src> TermReference<'src> for Term<'src> {
    type Variable = Variable<'src>;

    fn is_known(&self) -> bool {
        match &*self.0.borrow() {
            TermVariants::Value { term, .. } => !matches!(term, GenericTerm::Variable(_)),
            TermVariants::Link { target } => target.is_known(),
        }
    }

    fn typ(&self) -> Self {
        match &*self.0.borrow() {
            TermVariants::Value { pass, term, .. } => {
                term.typ(|e| Self::new(*pass, e), |var| var.typ.clone())
            }
            TermVariants::Link { target } => target.typ(),
        }
    }
}

impl<'src> VariableBinding<'src, Term<'src>> {
    fn infer_types(&mut self, pass: u64) -> Result<()> {
        self.variable_value.infer_types(pass)?;
        self.in_term.infer_types(pass)
    }

    fn make_fresh_variables(&mut self, new_variables: &mut OldToNewVariable<'src>) -> Self {
        let key = self.variable_value.0.as_ptr();
        let mut value = self.variable_value.value();

        let variable_value = if let Some(variable_value) = new_variables.get(&key) {
            drop(value);
            variable_value.clone()
        } else if let GenericTerm::Variable(variable) = &mut *value {
            // TODO: We need a debruijn level to see if `self` binds `self.variable_value`.
            // It could have been unified to another bound variable.
            let variable_value = Term::new(
                0,
                GenericTerm::Variable(Variable {
                    name: variable.name,
                    typ: variable.typ.make_fresh_variables(new_variables),
                }),
            );
            drop(value);
            new_variables.insert(key, variable_value.clone());
            variable_value
        } else {
            drop(value);
            self.variable_value.make_fresh_variables(new_variables)
        };

        let in_term = self.in_term.make_fresh_variables(new_variables);

        Self {
            name: self.name,
            binder: self.binder,
            variable_value,
            in_term,
        }
    }

    fn unify(binding0: &mut Self, binding1: &mut Self) -> Result<()> {
        Term::unify(&mut binding0.variable_value, &mut binding1.variable_value)?;
        Term::unify(&mut binding0.in_term, &mut binding1.in_term)
    }
}

impl<'src> GenericTerm<'src, Term<'src>> {
    fn infer_types(&mut self, pass: u64) -> Result<()> {
        match self {
            Self::TypeOfType => (),
            Self::Constant { typ, .. } => typ.infer_types(pass)?,
            Self::Apply {
                function,
                argument,
                typ,
            } => {
                Term::unify(
                    &mut typ.typ().fresh_variables(),
                    &mut Term::type_of_type(pass),
                )?;
                // TODO: Is this reasoning sound?
                // We're creating a new bound variable (`argument`) here. If it's unknown, we
                // want to infer it, so we don't want it to be fresh. Therefore we create fresh
                // variables for `argument` and `typ`, not `pi_type`.
                let pi_type =
                    &mut Term::pi_type(pass, argument.fresh_variables(), typ.fresh_variables());
                Term::unify(pi_type, &mut function.typ().fresh_variables())?;

                function.infer_types(pass)?;
                argument.infer_types(pass)?;
                typ.infer_types(pass)?;
            }
            Self::Variable(variable) => variable.typ.infer_types(pass)?,
            Self::VariableBinding(binding) => binding.infer_types(pass)?,
        }

        Ok(())
    }
}
