use std::{cell::RefMut, fmt, mem, ops::ControlFlow};

use clonelet::clone;
#[cfg(doc)]
use katexit::katexit;

use crate::{
    Evaluation, InferenceError, Pretty, Result, SharedMut, VariableBinding,
    constraints::Constraints,
};

mod pretty;

#[derive(Clone)]
pub struct Term<'src>(SharedMut<IndirectTerm<'src>>);

enum IndirectTerm<'src> {
    Value { term: TermEnum<'src> },
    Link { target: Term<'src> },
}

impl<'src> Term<'src> {
    pub fn type_of_type() -> Self {
        Self::new(TermEnum::Type)
    }

    pub fn unknown(typ: Self) -> Self {
        Self::new(TermEnum::Unknown { typ })
    }

    pub fn unknown_value() -> Self {
        Self::unknown(Self::unknown_type())
    }

    pub fn unknown_type() -> Self {
        Self::unknown(Self::type_of_type())
    }

    pub fn variable(name: Option<&'src str>, typ: Self) -> Self {
        Self::new(TermEnum::Variable {
            name,
            fresh: None,
            typ,
        })
    }

    pub fn constant(name: &'src str, value: Self) -> Self {
        Self::new(TermEnum::Constant { name, value })
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
        constraints: &Constraints<'src>,
    ) -> Self {
        constraints.add({
            clone!(function, argument, mut typ);

            async move {
                let variable = Self::variable(None, argument.typ());
                let mut function_type = Self::pi_type(variable, Self::unknown_type(), evaluation);
                Self::unify(&mut function_type, &mut function.typ()).await?;

                let mut result_type = if let TermEnum::Pi(binding) = &mut *function_type.value() {
                    binding.apply(&argument)
                } else {
                    panic!("Expected a PI type")
                };

                Self::unify(&mut typ, &mut result_type).await?;
                Ok(())
            }
        });

        typ.unify_type(constraints);

        Self::new(TermEnum::Apply {
            function,
            argument,
            typ,
            evaluation,
        })
    }

    pub fn has_type(self, mut typ: Self, constraints: &Constraints<'src>) -> Self {
        typ.unify_type(constraints);
        Self::add_unify_constraint(self.typ(), typ, constraints);
        self
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
    pub(crate) fn let_binding(
        value: Self,
        binding: VariableBinding<Self>,
        constraints: &Constraints<'src>,
    ) -> Self {
        Self::add_unify_constraint(value.typ(), binding.variable.typ(), constraints);
        Self::new(TermEnum::Let { value, binding })
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
    pub(crate) fn pi(mut binding: VariableBinding<Self>, constraints: &Constraints<'src>) -> Self {
        binding.in_term.unify_type(constraints);
        Self::new(TermEnum::Pi(binding))
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
    pub(crate) fn lambda(binding: VariableBinding<Self>) -> Self {
        Self::new(TermEnum::Lambda(binding))
    }

    pub(crate) fn variable_name(&mut self) -> Option<&'src str> {
        let TermEnum::Variable { name, .. } = &*self.value() else {
            return None;
        };

        *name
    }

    /// Create a copy of `self`, making a fresh variable for every binding.
    ///
    /// This allows us to modify the copy and substitute variables without
    /// affecting the original.
    fn fresh_variables(&mut self) -> Self {
        let mut value = self.value();

        Self::new(match &mut *value {
            TermEnum::Variable {
                fresh: Some(fresh), ..
            } => {
                return fresh.clone();
            }
            TermEnum::Variable { fresh: None, .. } | TermEnum::Unknown { .. } => {
                drop(value);
                return self.clone();
            }
            TermEnum::Constant { name, value } => TermEnum::Constant {
                name,
                value: value.fresh_variables(),
            },
            TermEnum::Let { value, binding } => TermEnum::Let {
                value: value.fresh_variables(),
                binding: binding.fresh_variables(),
            },
            TermEnum::Pi(binding) => TermEnum::Pi(binding.fresh_variables()),
            TermEnum::Lambda(binding) => TermEnum::Lambda(binding.fresh_variables()),
            TermEnum::Type => TermEnum::Type,
            TermEnum::Apply {
                function,
                argument,
                typ,
                evaluation,
            } => TermEnum::Apply {
                function: function.fresh_variables(),
                argument: argument.fresh_variables(),
                typ: typ.fresh_variables(),
                evaluation: *evaluation,
            },
        })
    }

    fn pi_type(variable: Self, result_type: Self, evaluation: Evaluation) -> Self {
        Self::new(TermEnum::Pi(VariableBinding {
            variable,
            in_term: result_type,
            evaluation,
        }))
    }

    fn new(term: TermEnum<'src>) -> Self {
        Self(SharedMut::new(IndirectTerm::Value { term }))
    }

    fn add_unify_constraint(mut x: Self, mut y: Self, constraints: &Constraints<'src>) {
        constraints.add(async move { Self::unify(&mut x, &mut y).await })
    }

    fn is_reducible(&mut self) -> bool {
        match &*self.value() {
            // TODO: Apply isn't always reducible
            TermEnum::Apply { .. } => true,
            TermEnum::Let { .. } => true,
            _ => false,
        }
    }

    fn unify_type(&mut self, constraints: &Constraints<'src>) {
        let mut typ = self.typ();

        constraints.add(async move { Self::unify(&mut typ, &mut Self::type_of_type()).await })
    }

    fn unify_unknown(
        &mut self,
        other: &mut Self,
        reducible: &mut Vec<(Self, Self)>,
    ) -> Result<ControlFlow<()>> {
        let mut typ = if let TermEnum::Unknown { typ } = &mut *self.value() {
            typ.clone()
        } else {
            return Ok(ControlFlow::Continue(()));
        };

        self.replace_with(other);
        Self::unify_upto_eval(&mut typ, &mut other.typ(), reducible)?;
        Ok(ControlFlow::Break(()))
    }

    fn unify_implicit_parameter(
        &mut self,
        other: &mut Self,
        reducible: &mut Vec<(Self, Self)>,
    ) -> Result<ControlFlow<()>> {
        if let TermEnum::Pi(binding) = &*other.value()
            && binding.evaluation == Evaluation::Static
        {
            return Ok(ControlFlow::Continue(()));
        }

        let mut in_term = if let TermEnum::Pi(binding) = &mut *self.value()
            && binding.evaluation == Evaluation::Static
        {
            binding.apply(&Self::unknown_value())
        } else {
            return Ok(ControlFlow::Continue(()));
        };

        Self::unify_upto_eval(&mut in_term, other, reducible)?;
        Ok(ControlFlow::Break(()))
    }

    fn unify_upto_eval(
        x: &mut Self,
        y: &mut Self,
        reducible: &mut Vec<(Self, Self)>,
    ) -> Result<()> {
        if Self::is_same(x, y)
            || x.unify_unknown(y, reducible)?.is_break()
            || y.unify_unknown(x, reducible)?.is_break()
            || x.unify_implicit_parameter(y, reducible)?.is_break()
            || y.unify_implicit_parameter(x, reducible)?.is_break()
        {
            return Ok(());
        }

        if Self::is_same(x, y) {
            return Ok(());
        }

        if x.is_reducible() || y.is_reducible() {
            reducible.push((x.clone(), y.clone()));
            return Ok(());
        }

        Self::unify_known(x, y, reducible)?;
        x.replace_with(y);

        Ok(())
    }

    async fn unify(x: &mut Self, y: &mut Self) -> Result<()> {
        let mut reducible = Vec::new();
        Self::unify_upto_eval(x, y, &mut reducible)?;

        while !reducible.is_empty() {
            for (mut x, mut y) in mem::take(&mut reducible) {
                // TODO: await unknowns
                x.evaluate()?;
                y.evaluate()?;
                Self::unify_upto_eval(&mut x, &mut y, &mut reducible)?;
            }
        }

        Ok(())
    }

    fn evaluate(&mut self) -> Result<()> {
        // TODO: This does evaluation by substitution, which is very inefficient. We
        // should use a stack and De Bruijn Indices.
        let mut borrow = self.value();

        match &mut *borrow {
            TermEnum::Apply {
                function, argument, ..
            } => {
                let reduced = function.evaluate_apply(argument)?;
                drop(borrow);
                self.replace_with(&reduced);
            }
            TermEnum::Let { value, binding } => {
                value.evaluate()?;
                let mut reduced = binding.apply(value);
                reduced.evaluate()?;
                drop(borrow);
                self.replace_with(&reduced);
            }
            _ => (),
        }

        Ok(())
    }

    fn evaluate_apply(&mut self, argument: &mut Self) -> Result<Self> {
        self.evaluate()?;
        argument.evaluate()?;
        let mut value = self.value();

        match &mut *value {
            TermEnum::Lambda(binding) => Ok(binding.apply(argument)),
            TermEnum::Apply { .. }
            | TermEnum::Let { .. }
            | TermEnum::Variable { .. }
            | TermEnum::Constant { .. } => {
                drop(value);
                Ok(self.clone())
            }
            TermEnum::Unknown { .. } => unreachable!("Expected Unknown to be inferred"),
            TermEnum::Pi(_) | TermEnum::Type => Err(InferenceError::UnexpectedTypeDuringEval)?,
        }
    }

    fn unify_known(x: &mut Self, y: &mut Self, reducible: &mut Vec<(Self, Self)>) -> Result<()> {
        let mut x_ref = x.try_value()?;
        let mut y_ref = y.try_value()?;

        // TODO: Can we use mutable borrowing to do the occurs check for us?
        match (&mut *x_ref, &mut *y_ref) {
            (TermEnum::Type, TermEnum::Type) => {}
            (
                TermEnum::Constant { name, value },
                TermEnum::Constant {
                    name: name1,
                    value: value1,
                },
            ) if name == name1 => Self::unify_upto_eval(value, value1, reducible)?,
            (
                TermEnum::Apply {
                    function,
                    argument,
                    typ,
                    evaluation,
                },
                TermEnum::Apply {
                    function: function1,
                    argument: argument1,
                    typ: typ1,
                    evaluation: evaluation1,
                },
            ) if evaluation == evaluation1 => {
                Self::unify_upto_eval(function, function1, reducible)?;
                Self::unify_upto_eval(argument, argument1, reducible)?;
                Self::unify_upto_eval(typ, typ1, reducible)?;
            }
            (
                TermEnum::Let { value, binding },
                TermEnum::Let {
                    value: value1,
                    binding: binding1,
                },
            ) => {
                Self::unify_upto_eval(value, value1, reducible)?;
                VariableBinding::unify(binding, binding1, reducible)?
            }
            (TermEnum::Pi(binding0), TermEnum::Pi(binding1))
                if binding0.evaluation == binding1.evaluation =>
            {
                VariableBinding::unify(binding0, binding1, reducible)?
            }
            (TermEnum::Lambda(binding0), TermEnum::Lambda(binding1)) => {
                VariableBinding::unify(binding0, binding1, reducible)?
            }
            // It's safer to explicitly ignore each variant
            (TermEnum::Type, _rhs)
            | (TermEnum::Apply { .. }, _rhs)
            | (TermEnum::Variable { .. }, _rhs)
            | (TermEnum::Constant { .. }, _rhs)
            | (TermEnum::Unknown { .. }, _rhs)
            | (TermEnum::Let { .. }, _rhs)
            | (TermEnum::Pi(_), _rhs)
            | (TermEnum::Lambda(_), _rhs) => Err(InferenceError::CouldntUnify)?,
        }

        Ok(())
    }

    fn is_same(x: &mut Self, y: &mut Self) -> bool {
        x.collapse_links();
        y.collapse_links();

        SharedMut::is_same(&x.0, &y.0)
    }

    fn replace_with(&mut self, other: &Self) {
        self.collapse_links();
        self.0.replace_with(IndirectTerm::Link {
            target: other.clone(),
        });
        *self = other.clone();
    }

    fn collapse_links(&mut self) {
        self.try_collapse_links().unwrap()
    }

    fn try_collapse_links(&mut self) -> Result<()> {
        // Collapse links from the bottom up so they are also collapsed for other
        // terms that reference this chain.

        *self = {
            let mut borrow = self
                .0
                .try_borrow_mut()
                .map_err(|_| InferenceError::InfiniteTerm)?;

            let IndirectTerm::Link { target } = &mut *borrow else {
                return Ok(());
            };
            target.collapse_links();
            target.clone()
        };

        Ok(())
    }

    fn value<'a>(&'a mut self) -> RefMut<'a, TermEnum<'src>> {
        self.try_value().unwrap()
    }

    fn try_value<'a>(&'a mut self) -> Result<RefMut<'a, TermEnum<'src>>> {
        self.try_collapse_links()?;
        let borrow = self
            .0
            .try_borrow_mut()
            .map_err(|_| InferenceError::InfiniteTerm)?;

        Ok(RefMut::map(borrow, |x| match x {
            IndirectTerm::Value { term, .. } => term,
            IndirectTerm::Link { .. } => {
                unreachable!("Links should be collapsed at this point")
            }
        }))
    }

    fn is_known(&self) -> bool {
        !matches!(&*self.clone().value(), TermEnum::Unknown { .. })
    }

    fn typ(&self) -> Self {
        match &*self.clone().value() {
            TermEnum::Type | TermEnum::Pi(_) => Term::type_of_type(),
            TermEnum::Apply { typ, .. }
            | TermEnum::Unknown { typ }
            | TermEnum::Variable { typ, .. } => typ.clone(),
            TermEnum::Constant { value, .. } => value.typ(),
            TermEnum::Let { binding, .. } => binding.in_term.typ(),
            TermEnum::Lambda(binding) => Term::pi_type(
                binding.variable.clone(),
                binding.in_term.typ(),
                binding.evaluation,
            ),
        }
    }
}

impl fmt::Debug for Term<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.debug(f)
    }
}

enum TermEnum<'src> {
    Type,
    Apply {
        function: Term<'src>,
        argument: Term<'src>,
        typ: Term<'src>,
        evaluation: Evaluation,
    },
    Variable {
        name: Option<&'src str>,
        fresh: Option<Term<'src>>,
        typ: Term<'src>,
    },
    Constant {
        name: &'src str,
        value: Term<'src>,
    },
    Unknown {
        typ: Term<'src>,
    },
    Let {
        value: Term<'src>,
        binding: VariableBinding<Term<'src>>,
    },
    Pi(VariableBinding<Term<'src>>),
    Lambda(VariableBinding<Term<'src>>),
}

impl<'src> VariableBinding<Term<'src>> {
    pub fn variable_name(&self) -> Option<&'src str> {
        self.variable.clone().variable_name()
    }

    fn apply(&mut self, argument: &Term<'src>) -> Term<'src> {
        // We only need to substitute `self.variable` and copy everything else, but
        // allocating `fresh_variables` is:
        //
        // - Safer as we don't have to make sure we copy other variables when we
        //   substitute them
        // - Easier as we don't have to write `substitute` methods
        let Self {
            mut variable,
            in_term,
            ..
        } = self.fresh_variables();

        variable.replace_with(argument);
        in_term
    }

    fn fresh_variables(&mut self) -> Self {
        let variable = self.allocate_fresh_variable();
        let in_term = self.in_term.fresh_variables();
        self.free_fresh_variable();

        Self {
            variable,
            in_term,
            evaluation: self.evaluation,
        }
    }

    fn allocate_fresh_variable(&mut self) -> Term<'src> {
        let TermEnum::Variable { name, fresh, typ } = &mut *self.variable.value() else {
            return self.variable.fresh_variables();
        };

        let variable = Term::variable(*name, typ.fresh_variables());

        assert!(fresh.is_none());
        *fresh = Some(variable.clone());
        variable
    }

    fn free_fresh_variable(&mut self) {
        if let TermEnum::Variable { fresh, .. } = &mut *self.variable.value() {
            *fresh = None;
        }
    }

    fn unify(
        binding0: &mut Self,
        binding1: &mut Self,
        reducible: &mut Vec<(Term<'src>, Term<'src>)>,
    ) -> Result<()> {
        if binding0.evaluation != binding1.evaluation {
            return Err(InferenceError::CouldntUnify);
        }

        Term::unify_upto_eval(
            &mut binding0.variable.typ(),
            &mut binding1.variable.typ(),
            reducible,
        )?;

        // Keep the name if we can
        if binding0.variable_name().is_some() {
            binding1.variable.replace_with(&binding0.variable);
        } else {
            binding0.variable.replace_with(&binding1.variable);
        }

        Term::unify_upto_eval(&mut binding0.in_term, &mut binding1.in_term, reducible)
    }
}
