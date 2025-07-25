use std::{cell::RefMut, fmt, ops::ControlFlow};

#[cfg(doc)]
use katexit::katexit;

use crate::{
    Evaluation, GenericTerm, InferenceError, Pretty, Result, SharedMut, TermReference,
    VariableBinding,
};

mod pretty;

#[derive(Clone)]
pub struct Term<'src>(SharedMut<TermEnum<'src>>);

enum TermEnum<'src> {
    Value { term: GenericTerm<'src, Term<'src>> },
    // TODO: Can links cause circular references?
    Link { target: Term<'src> },
}

impl<'src> Term<'src> {
    pub fn type_of_type() -> Self {
        Self::new(GenericTerm::TypeOfType)
    }

    pub fn unknown(typ: Self) -> Self {
        Self::new(GenericTerm::Unknown { typ })
    }

    pub fn unknown_value() -> Self {
        Self::unknown(Self::unknown_type())
    }

    pub fn unknown_type() -> Self {
        Self::unknown(Self::type_of_type())
    }

    pub fn variable(name: Option<&'src str>, typ: Self) -> Self {
        Self::new(GenericTerm::Variable {
            name: VariableName::new(name),
            typ,
        })
    }

    pub fn constant(name: &'src str, typ: Self) -> Result<Self> {
        typ.unify_type()?;
        Ok(Self::new(GenericTerm::Constant { name, typ }))
    }

    #[cfg_attr(doc, katexit)]
    /// # Application
    ///
    /// This is the elimination rule for $\lambda$ abstraction:
    ///
    /// $$
    /// \dfrac{
    ///     \Gamma \vdash f : \Pi x : A . \\: B \qquad \Gamma \vdash a : A
    /// } {
    ///     \Gamma \vdash f \\: a : B[a/x]
    /// }
    /// $$
    ///
    /// The term $B[a/x]$ means replace $x$ with $a$ in $B$ (which is necessary
    /// as $B$ depends on $x$ in the premise).
    pub fn apply(
        function: Self,
        argument: Self,
        mut typ: Self,
        evaluation: Evaluation,
    ) -> Result<Self> {
        let mut extract_arg = Self::unknown_value();
        let pi_type = &mut Term::pi_type(extract_arg.clone(), typ.fresh_variables(), evaluation);
        Self::unify(pi_type, &mut function.typ().fresh_variables())?;

        // TODO: Only do this for variables (assert?):
        Self::unify(
            &mut extract_arg.typ(),
            &mut argument.typ().fresh_variables(),
        )?;
        extract_arg.replace_with(&argument);
        typ.unify_type()?;

        Ok(Self::new(GenericTerm::Apply {
            function,
            argument,
            typ,
            evaluation,
        }))
    }

    pub fn has_type(self, mut typ: Self) -> Result<Self> {
        typ.unify_type()?;
        Self::unify(&mut self.typ(), &mut typ)?;
        Ok(self)
    }

    #[cfg_attr(doc, katexit)]
    /// # Let
    ///
    /// $$
    /// \dfrac{
    ///     \Gamma \vdash a : A \qquad \Gamma, x : A \vdash e : B
    /// }{
    ///     \Gamma \vdash \text{let } x : A = a \text{ in } e : B[a/x]
    /// }
    /// $$
    pub(crate) fn let_binding(value: Self, binding: VariableBinding<'src, Self>) -> Result<Self> {
        Self::unify(&mut value.typ(), &mut binding.variable.typ())?;
        Ok(Self::new(GenericTerm::Let { value, binding }))
    }

    #[cfg_attr(doc, katexit)]
    /// # $\Pi$ Type Introduction
    ///
    /// $\Pi \\: Type$s are not analyzed in standard dependent type theory, so
    /// they don't have an elimination rule.
    ///
    /// $$
    /// \dfrac{
    ///     \Gamma \vdash A : \text{Type}_i \qquad \Gamma, x : A \vdash B :
    /// \text{Type}_i } {
    ///     \Gamma \vdash \Pi x : A . \\: B : \text{Type}_i
    /// }
    /// $$
    ///
    /// We don't use indexed type universes, as this isn't a theorem proving
    /// language.
    pub(crate) fn pi(binding: VariableBinding<'src, Self>) -> Self {
        Self::new(GenericTerm::Pi(binding))
    }

    #[cfg_attr(doc, katexit)]
    /// # Abstraction
    ///
    /// The introduction rule for $\lambda$ abstractions:
    ///
    /// $$
    /// \dfrac{
    ///     \Gamma, x : A \vdash e : B
    /// } {
    ///     \Gamma \vdash \lambda x : A . \\: e : \Pi x : A . \\: B
    /// }
    /// $$
    pub(crate) fn lambda(binding: VariableBinding<'src, Self>) -> Self {
        Self::new(GenericTerm::Lambda(binding))
    }

    fn fresh_variables(&mut self) -> Self {
        let mut value = self.value();

        Self::new(match &mut *value {
            GenericTerm::Variable {
                name: VariableName {
                    fresh: Some(fresh), ..
                },
                ..
            } => {
                return fresh.clone();
            }
            GenericTerm::Variable { .. } | GenericTerm::Unknown { .. } => {
                drop(value);
                return self.clone();
            }
            GenericTerm::Let { value, binding } => GenericTerm::Let {
                value: value.fresh_variables(),
                binding: binding.fresh_variables(),
            },
            GenericTerm::Pi(binding) => GenericTerm::Pi(binding.fresh_variables()),
            GenericTerm::Lambda(binding) => GenericTerm::Lambda(binding.fresh_variables()),
            GenericTerm::TypeOfType => GenericTerm::TypeOfType,
            GenericTerm::Constant { name, typ } => GenericTerm::Constant {
                name,
                typ: typ.fresh_variables(),
            },
            GenericTerm::Apply {
                function,
                argument,
                typ,
                evaluation,
            } => GenericTerm::Apply {
                function: function.fresh_variables(),
                argument: argument.fresh_variables(),
                typ: typ.fresh_variables(),
                evaluation: *evaluation,
            },
        })
    }

    fn pi_type(argument_value: Self, result_type: Self, evaluation: Evaluation) -> Self {
        Self::new(GenericTerm::pi(
            None,
            argument_value,
            result_type,
            evaluation,
        ))
    }

    fn new(term: GenericTerm<'src, Self>) -> Self {
        Self(SharedMut::new(TermEnum::Value { term }))
    }

    fn unify_type(&self) -> Result<()> {
        Self::unify(&mut self.typ(), &mut Self::type_of_type())
    }

    fn unify_unknown(&mut self, other: &mut Self) -> Result<ControlFlow<()>> {
        if other.value().is_variable() {
            return Ok(ControlFlow::Continue(()));
        }

        let mut self_ref = self.value();

        Ok(if let GenericTerm::Unknown { typ } = &mut *self_ref {
            let mut typ = typ.clone();
            drop(self_ref);
            self.replace_with(other);
            Self::unify(&mut typ, &mut other.typ())?;

            ControlFlow::Break(())
        } else {
            ControlFlow::Continue(())
        })
    }

    fn unify(x: &mut Self, y: &mut Self) -> Result<()> {
        x.collapse_links();
        y.collapse_links();

        if SharedMut::is_same(&x.0, &y.0)
            || x.unify_unknown(y)?.is_break()
            || y.unify_unknown(x)?.is_break()
        {
            return Ok(());
        }

        let mut x_ref = x.value();
        let mut y_ref = y.value();

        // TODO: Application evaluation:
        //
        // Can we use `Future`s to manage all of this for us? `unify` and `evaluate`
        // would be `async`. `unify` would `await` `evaluate`, and `evaluate` would
        // `await` for `Unknown` values to change. `TermEnum::Value` would need to wrap
        // its term in a `tokio::sync::watch`, or contain a `tokio::sync::Notify` or
        // something similar. We could box this and make it optional for efficiency. We
        // need to consider whether this should be on `TermEnum::Value`or
        // `GenericTerm::Unknown`. Any `GenericTerm` variant could be updated when we
        // unify a compile time application.
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
                GenericTerm::Constant { name, typ },
                GenericTerm::Constant {
                    name: name1,
                    typ: typ1,
                },
            ) if name == name1 => Self::unify(typ, typ1)?,
            (
                GenericTerm::Apply {
                    function,
                    argument,
                    typ,
                    evaluation,
                },
                GenericTerm::Apply {
                    function: function1,
                    argument: argument1,
                    typ: typ1,
                    evaluation: evaluation1,
                },
            ) if evaluation == evaluation1 => {
                Self::unify(function, function1)?;
                Self::unify(argument, argument1)?;
                Self::unify(typ, typ1)?;
            }
            (
                GenericTerm::Let { value, binding },
                GenericTerm::Let {
                    value: value1,
                    binding: binding1,
                },
            ) => {
                Self::unify(value, value1)?;
                VariableBinding::unify(binding, binding1)?
            }
            (GenericTerm::Pi(binding0), GenericTerm::Pi(binding1))
                if binding0.evaluation == binding1.evaluation =>
            {
                VariableBinding::unify(binding0, binding1)?
            }
            (GenericTerm::Lambda(binding0), GenericTerm::Lambda(binding1)) => {
                VariableBinding::unify(binding0, binding1)?
            }
            // It's safer to explicitly ignore each variant
            (GenericTerm::TypeOfType, _rhs)
            | (GenericTerm::Constant { .. }, _rhs)
            | (GenericTerm::Apply { .. }, _rhs)
            | (GenericTerm::Variable { .. }, _rhs)
            | (GenericTerm::Unknown { .. }, _rhs)
            | (GenericTerm::Let { .. }, _rhs)
            | (GenericTerm::Pi(_), _rhs)
            | (GenericTerm::Lambda(_), _rhs) => Err(InferenceError)?,
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

pub struct VariableName<'src> {
    name: Option<&'src str>,
    fresh: Option<Term<'src>>,
}

impl<'src> VariableName<'src> {
    fn new(name: Option<&'src str>) -> Self {
        Self { name, fresh: None }
    }
}

impl<'src> TermReference<'src> for Term<'src> {
    type VariableName = VariableName<'src>;

    fn is_known(&self) -> bool {
        match &*self.0.borrow() {
            TermEnum::Value { term, .. } => term.is_known(),
            TermEnum::Link { target } => target.is_known(),
        }
    }

    fn typ(&self) -> Self {
        match &*self.0.borrow() {
            TermEnum::Value { term, .. } => term.typ(Self::new),
            TermEnum::Link { target } => target.typ(),
        }
    }
}

impl<'src> VariableBinding<'src, Term<'src>> {
    fn fresh_variables(&mut self) -> Self {
        let variable = self.allocate_fresh_variable();
        let in_term = self.in_term.fresh_variables();
        self.free_fresh_variable();

        Self {
            name: self.name,
            variable,
            in_term,
            evaluation: self.evaluation,
        }
    }

    fn allocate_fresh_variable(&mut self) -> Term<'src> {
        let GenericTerm::Variable { name, typ } = &mut *self.variable.value() else {
            return self.variable.fresh_variables();
        };

        let variable = Term::new(GenericTerm::Variable {
            name: VariableName::new(name.name),
            typ: typ.fresh_variables(),
        });

        assert!(name.fresh.is_none());
        name.fresh = Some(variable.clone());
        variable
    }

    fn free_fresh_variable(&mut self) {
        if let GenericTerm::Variable { name, .. } = &mut *self.variable.value() {
            name.fresh = None;
        }
    }

    fn unify(binding0: &mut Self, binding1: &mut Self) -> Result<()> {
        if binding0.evaluation != binding1.evaluation {
            return Err(InferenceError);
        }

        Term::unify(&mut binding0.variable.typ(), &mut binding1.variable.typ())?;

        // Keep the name if we can
        if binding0.name.is_some() {
            binding1.variable.replace_with(&binding0.variable);
            binding1.name = binding0.name;
        } else {
            binding0.variable.replace_with(&binding1.variable);
            binding0.name = binding1.name;
        }

        Term::unify(&mut binding0.in_term, &mut binding1.in_term)
    }
}
