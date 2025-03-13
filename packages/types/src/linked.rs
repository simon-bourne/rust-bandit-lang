use std::{cell::RefMut, collections::HashMap, fmt, ops::ControlFlow, rc::Rc};

use crate::{
    GenericTerm, InferenceError, Pretty, Result, SharedMut, TermReference, VariableBinding,
    VariableValue,
};

mod pretty;

#[derive(Clone)]
pub struct Term<'src>(SharedMut<TermEnum<'src>>);

enum TermEnum<'src> {
    Value { term: GenericTerm<'src, Term<'src>> },
    // TODO: Can links cause circular references?
    Link { target: Term<'src> },
}

type OldToNewVariable<'src> = HashMap<*const (), Term<'src>>;

impl<'src> Term<'src> {
    pub fn type_of_type() -> Self {
        Self::new(GenericTerm::TypeOfType)
    }

    pub fn unknown(id: Option<VariableId<'src>>, typ: Self) -> Self {
        Self::new(GenericTerm::Variable(Variable {
            id,
            value: VariableValue::Unknown { typ },
        }))
    }

    pub fn unknown_value() -> Self {
        Self::unknown(None, Self::unknown_type())
    }

    pub fn unknown_type() -> Self {
        Self::unknown(None, Self::type_of_type())
    }

    pub fn variable(id: Option<VariableId<'src>>, value: Self) -> Self {
        Self::new(GenericTerm::Variable(Variable {
            id,
            value: VariableValue::Known { value },
        }))
    }

    pub fn constant(name: &'src str, typ: Self) -> Result<Self> {
        Term::unify(&mut typ.typ(), &mut Term::type_of_type())?;
        Ok(Self::new(GenericTerm::Constant { name, typ }))
    }

    pub fn apply(function: Self, mut argument: Self, mut typ: Self) -> Result<Self> {
        // We're creating a new bound variable (`argument`) here. If it's unknown, we
        // want to infer it, so we don't want it to be fresh. Therefore we create fresh
        // variables for `argument` and `typ`, not `pi_type`.
        let pi_type = &mut Term::pi_type(argument.fresh_variables(), typ.fresh_variables());
        Term::unify(pi_type, &mut function.typ().fresh_variables())?;
        Term::unify(&mut typ.typ(), &mut Term::type_of_type())?;

        Ok(Self::new(GenericTerm::Apply {
            function,
            argument,
            typ,
        }))
    }

    pub fn has_type(self, mut typ: Self) -> Result<Self> {
        Self::unify(&mut typ.typ(), &mut Self::type_of_type())?;
        Self::unify(&mut self.typ(), &mut typ)?;
        Ok(self)
    }

    pub(crate) fn binding(binding: VariableBinding<'src, Self>) -> Self {
        Self::new(GenericTerm::VariableBinding(binding))
    }

    // TODO: The implementation of this is ugly and inefficient.
    pub fn fresh_variables(&mut self) -> Self {
        self.make_fresh_variables(&mut HashMap::new())
    }

    fn make_fresh_variables(&mut self, new_variables: &mut OldToNewVariable<'src>) -> Self {
        let mut value = self.value();

        Self::new(match &mut *value {
            GenericTerm::Variable(variable) => {
                let fresh = if let Some(new_variable) =
                    variable.id().and_then(|id| new_variables.get(&id.key()))
                {
                    new_variable.clone()
                } else {
                    drop(value);
                    self.clone()
                };

                return fresh;
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
        })
    }

    fn pi_type(argument_value: Self, result_type: Self) -> Self {
        Self::new(GenericTerm::pi(None, argument_value, result_type))
    }

    fn new(term: GenericTerm<'src, Self>) -> Self {
        Self(SharedMut::new(TermEnum::Value { term }))
    }

    fn unify_named_variable(&mut self, other: &mut Self) -> Result<ControlFlow<()>> {
        let mut self_ref = self.value();
        let mut other_ref = other.value();

        Ok(match (&mut *self_ref, &mut *other_ref) {
            (
                GenericTerm::Variable(Variable { id: Some(_), value }),
                GenericTerm::Variable(Variable { id: None, .. }),
            ) => {
                let mut value = value.clone();
                drop((self_ref, other_ref));
                other.replace_with(self);
                value.unify(other)?;

                ControlFlow::Break(())
            }
            (GenericTerm::Variable(Variable { id: Some(_), value }), _) => {
                // TODO: Prefer variables with smaller scope
                let mut value = value.clone();
                drop((self_ref, other_ref));
                self.replace_with(other);
                value.unify(other)?;

                ControlFlow::Break(())
            }
            _ => ControlFlow::Continue(()),
        })
    }

    fn unify_variable(&mut self, other: &mut Self) -> Result<ControlFlow<()>> {
        let mut self_ref = self.value();

        Ok(
            if let GenericTerm::Variable(Variable { value, .. }) = &mut *self_ref {
                let mut value = value.clone();
                drop(self_ref);
                self.replace_with(other);
                value.unify(other)?;

                ControlFlow::Break(())
            } else {
                ControlFlow::Continue(())
            },
        )
    }

    fn unify(x: &mut Self, y: &mut Self) -> Result<()> {
        x.collapse_links();
        y.collapse_links();

        if SharedMut::is_same(&x.0, &y.0)
            || x.unify_named_variable(y)?.is_break()
            || y.unify_named_variable(x)?.is_break()
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

        drop((x_ref, y_ref));
        x.replace_with(y);

        Ok(())
    }

    fn replace_with(&mut self, other: &Self) {
        self.collapse_links();
        self.0.replace_with(TermEnum::Link {
            target: other.clone(),
        });
        *self = other.clone();
    }

    fn collapse_links(&mut self) {
        // Collapse links from the bottom up so they are also collapsed for other
        // terms that reference this chain.
        *self = {
            let TermEnum::Link { target } = &mut *self.0.borrow_mut() else {
                return;
            };
            target.collapse_links();
            target.clone()
        };
    }

    fn value<'a>(&'a mut self) -> RefMut<'a, GenericTerm<'src, Self>> {
        self.collapse_links();
        RefMut::map(self.0.borrow_mut(), |x| match x {
            TermEnum::Value { term, .. } => term,
            TermEnum::Link { .. } => unreachable!("Links should be collapsed at this point"),
        })
    }
}

impl fmt::Debug for Term<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.debug(f)
    }
}

pub struct Variable<'src> {
    id: Option<VariableId<'src>>,
    value: VariableValue<Term<'src>>,
}

impl<'src> Variable<'src> {
    fn id(&self) -> Option<&VariableId<'src>> {
        self.id.as_ref()
    }

    fn typ(&self) -> Term<'src> {
        match &self.value {
            VariableValue::Known { value } => value.typ(),
            VariableValue::Unknown { typ } => typ.clone(),
        }
    }

    fn fresh(&mut self, new_variables: &mut OldToNewVariable<'src>) -> Self {
        let value = match &mut self.value {
            VariableValue::Known { value } => VariableValue::Known {
                value: value.make_fresh_variables(new_variables),
            },
            VariableValue::Unknown { typ } => VariableValue::Unknown {
                typ: typ.make_fresh_variables(new_variables),
            },
        };

        Self {
            id: self.id().map(VariableId::fresh),
            value,
        }
    }
}

impl<'src> VariableValue<Term<'src>> {
    fn unify(&mut self, other: &mut Term<'src>) -> Result<()> {
        match self {
            Self::Known { value } => Term::unify(value, other),
            Self::Unknown { typ } => Term::unify(typ, &mut other.typ()),
        }
    }
}

#[derive(Clone, Eq)]
pub struct VariableId<'src> {
    name: &'src str,
    id: Rc<()>,
}

impl PartialEq for VariableId<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && Rc::ptr_eq(&self.id, &other.id)
    }
}

impl<'src> VariableId<'src> {
    pub fn new(name: &'src str) -> Self {
        Self {
            name,
            id: Rc::new(()),
        }
    }

    fn fresh(&self) -> Self {
        Self::new(self.name)
    }

    fn key(&self) -> *const () {
        Rc::as_ptr(&self.id)
    }
}

impl AsRef<str> for VariableId<'_> {
    fn as_ref(&self) -> &str {
        self.name
    }
}

impl<'src> TermReference<'src> for Term<'src> {
    type Type = Self;
    type VariableId = VariableId<'src>;
    type VariableReference = Variable<'src>;
    type VariableValue = Self;

    fn is_known(&self) -> bool {
        match &*self.0.borrow() {
            TermEnum::Value { term, .. } => !matches!(
                term,
                GenericTerm::Variable(Variable {
                    value: VariableValue::Unknown { .. },
                    ..
                })
            ),
            TermEnum::Link { target } => target.is_known(),
        }
    }

    fn typ(&self) -> Self {
        match &*self.0.borrow() {
            TermEnum::Value { term, .. } => term.typ(Self::new, Variable::typ),
            TermEnum::Link { target } => target.typ(),
        }
    }
}

impl<'src> VariableBinding<'src, Term<'src>> {
    fn make_fresh_variables(&mut self, new_variables: &mut OldToNewVariable<'src>) -> Self {
        let mut value = self.variable_value.value();
        let new_id;
        let variable_value;

        match &mut *value {
            GenericTerm::Variable(variable) if self.id == variable.id => {
                let new_variable = variable.fresh(new_variables);
                new_id = new_variable.id.clone();
                variable_value = Term::new(GenericTerm::Variable(new_variable));
                drop(value);

                if let Some(id) = &self.id {
                    let existing = new_variables.insert(id.key(), variable_value.clone());
                    assert!(existing.is_none(), "Found out of scope variable");
                }
            }
            _ => {
                drop(value);
                new_id = self.id.clone();
                variable_value = self.variable_value.make_fresh_variables(new_variables);
            }
        };

        let in_term = self.in_term.make_fresh_variables(new_variables);

        Self {
            id: new_id,
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
