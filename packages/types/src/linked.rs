use std::{cell::RefMut, fmt, ops::ControlFlow};

use clonelet::clone;
#[cfg(doc)]
use katexit::katexit;

use crate::{
    Evaluation, GenericTerm, InferenceError, Pretty, Result, SharedMut, TermReference,
    VariableBinding, constraints::Constraints,
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
    pub fn typ() -> Self {
        Self::new(GenericTerm::Type)
    }

    pub fn unknown(typ: Self) -> Self {
        Self::new(GenericTerm::Unknown { typ })
    }

    pub fn unknown_value() -> Self {
        Self::unknown(Self::unknown_type())
    }

    pub fn unknown_type() -> Self {
        Self::unknown(Self::typ())
    }

    pub fn local_variable(name: Option<&'src str>, typ: Self) -> Self {
        Self::new(GenericTerm::Variable(Variable::local(name, typ)))
    }

    pub fn constant(name: &'src str, value: Self) -> Self {
        Self::new(GenericTerm::Variable(Variable::Global { name, value }))
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
            // We need fresh variables as we'll be instantiating the variable bound by
            // `function.typ()` with `argument`. We don't want to modify any linked
            // occurrences of `function`.
            let mut function_type = function.typ().fresh_variables();
            clone!(argument, typ);

            async move {
                let mut variable = Self::local_variable(None, Self::unknown_type());
                Self::unify(
                    &mut Term::pi_type(variable.clone(), typ, evaluation),
                    &mut function_type,
                )
                .await?;
                Self::unify(&mut variable.typ(), &mut argument.typ()).await?;
                variable.replace_with(&argument);
                Ok(())
            }
        });

        typ.unify_type(constraints);

        Self::new(GenericTerm::Apply {
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
        Self::new(GenericTerm::Let { value, binding })
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
    pub(crate) fn pi(binding: VariableBinding<Self>) -> Self {
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
    pub(crate) fn lambda(binding: VariableBinding<Self>) -> Self {
        Self::new(GenericTerm::Lambda(binding))
    }

    /// Create a copy of `self`, making a fresh variable for every binding.
    ///
    /// Note that we can't just substitute a single variable, because variables
    /// have a type that may depend on the variable we're substituting.
    ///
    /// For example, if we substitute `a` in the term `f : (a : Type) -> ((x :
    /// a) -> (f x))`, we also need a fresh variable for `x`.
    fn fresh_variables(&mut self) -> Self {
        let mut value = self.value();

        Self::new(match &mut *value {
            GenericTerm::Variable(Variable::Local {
                fresh: Some(fresh), ..
            }) => {
                return fresh.clone();
            }
            GenericTerm::Variable(Variable::Local { .. }) | GenericTerm::Unknown { .. } => {
                drop(value);
                return self.clone();
            }
            GenericTerm::Variable(Variable::Global { name, value }) => {
                GenericTerm::Variable(Variable::Global {
                    name,
                    value: value.fresh_variables(),
                })
            }
            GenericTerm::Let { value, binding } => GenericTerm::Let {
                value: value.fresh_variables(),
                binding: binding.fresh_variables(),
            },
            GenericTerm::Pi(binding) => GenericTerm::Pi(binding.fresh_variables()),
            GenericTerm::Lambda(binding) => GenericTerm::Lambda(binding.fresh_variables()),
            GenericTerm::Type => GenericTerm::Type,
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

    fn pi_type(bound_variable: Self, result_type: Self, evaluation: Evaluation) -> Self {
        Self::new(GenericTerm::pi(bound_variable, result_type, evaluation))
    }

    fn new(term: GenericTerm<'src, Self>) -> Self {
        Self(SharedMut::new(TermEnum::Value { term }))
    }

    fn is_local_variable(&mut self) -> bool {
        matches!(
            &*self.value(),
            GenericTerm::Variable(Variable::Local { .. })
        )
    }

    fn unify_type(&mut self, constraints: &Constraints<'src>) {
        let mut typ = self.typ();

        constraints.add(async move { Self::unify(&mut typ, &mut Self::typ()).await })
    }

    async fn unify_unknown(&mut self, other: &mut Self) -> Result<ControlFlow<()>> {
        if other.is_local_variable() {
            return Ok(ControlFlow::Continue(()));
        }

        let mut typ = if let GenericTerm::Unknown { typ } = &mut *self.value() {
            typ.clone()
        } else {
            return Ok(ControlFlow::Continue(()));
        };

        self.replace_with(other);
        Self::unify_recurse(&mut typ, &mut other.typ()).await?;
        Ok(ControlFlow::Break(()))
    }

    async fn unify_implicit_parameter(&mut self, other: &mut Self) -> Result<ControlFlow<()>> {
        if let GenericTerm::Pi(binding) = &*other.value()
            && binding.evaluation == Evaluation::Static
        {
            return Ok(ControlFlow::Continue(()));
        }

        let (mut variable, mut in_term) = if let GenericTerm::Pi(binding) = &mut *self.value()
            && binding.evaluation == Evaluation::Static
        {
            (binding.variable.clone(), binding.in_term.clone())
        } else {
            return Ok(ControlFlow::Continue(()));
        };

        variable.replace_with(&Self::unknown_value());
        Self::unify_recurse(&mut in_term, other).await?;
        // We replaced the variable, so make sure we don't leave the binding around
        self.replace_with(other);
        Ok(ControlFlow::Break(()))
    }

    fn add_unify_constraint(mut x: Self, mut y: Self, constraints: &Constraints<'src>) {
        constraints.add(async move { Self::unify(&mut x, &mut y).await })
    }

    async fn unify_recurse(x: &mut Self, y: &mut Self) -> Result<()> {
        Box::pin(Self::unify(x, y)).await
    }

    async fn unify(x: &mut Self, y: &mut Self) -> Result<()> {
        if Self::is_same(x, y)
            || x.unify_unknown(y).await?.is_break()
            || y.unify_unknown(x).await?.is_break()
            // TODO: Does this belong before or after evaluate?
            || x.unify_implicit_parameter(y).await?.is_break()
            || y.unify_implicit_parameter(x).await?.is_break()
        {
            return Ok(());
        }

        // TODO: Should we evaluate here? We're recursing down and evaluating here, then
        // doing the same at each level when unifying. Maybe it's better to `evaluate`
        // once before unification? Or do we want to evaluate bit by bit as we unify
        // each level? We should be able to infer more types that way.
        x.evaluate_node().await?;
        y.evaluate_node().await?;

        if Self::is_same(x, y) {
            return Ok(());
        }

        Self::unify_known(x, y).await?;
        x.replace_with(y);

        Ok(())
    }

    async fn evaluate_node(&mut self) -> Result<()> {
        // This lint gives a false positive. All the `match` arms below `drop(borrowed)`
        // before awaiting
        #![allow(clippy::await_holding_refcell_ref)]

        // TODO: Wait for unknowns to be evaluated
        let borrowed = self.value();

        match &*borrowed {
            GenericTerm::Apply {
                function, argument, ..
            } => {
                clone!(function, argument);
                drop(borrowed);
                self.evaluate_apply(function, argument).await?;
                Box::pin(self.evaluate_node()).await?;
            }
            GenericTerm::Let {
                value,
                binding:
                    VariableBinding {
                        variable, in_term, ..
                    },
            } => {
                clone!(variable, value, in_term);
                drop(borrowed);
                self.evaluate_let(variable, value, in_term).await?;
                Box::pin(self.evaluate_node()).await?;
            }
            _ => {}
        }

        Ok(())
    }

    async fn evaluate_apply(&mut self, mut function: Self, argument: Self) -> Result<()> {
        Box::pin(function.evaluate_node()).await?;

        // We need fresh variables if the function is a lambda, because we'll be
        // substituting the lambda's argument with a variable from outside the function
        // term.
        //
        // For example, in the term `(\x ⇒ x) y`, the child term `(\x ⇒ x)` could be
        // shared with other terms, so we want to modify a copy of it.
        match &mut *function.fresh_variables().value() {
            GenericTerm::Lambda(VariableBinding {
                variable, in_term, ..
            }) => {
                variable.replace_with(&argument);
                self.replace_with(in_term);
            }
            GenericTerm::Apply { .. } | GenericTerm::Let { .. } => {}
            GenericTerm::Variable { .. } => {}
            GenericTerm::Unknown { .. } => unreachable!("Expected Unknown to be inferred"),
            GenericTerm::Pi(_) | GenericTerm::Type => Err(InferenceError)?,
        }

        Ok(())
    }

    /// Evaluate an expression of the form `let variable = value in expression`
    async fn evaluate_let(&mut self, mut variable: Self, value: Self, in_term: Self) -> Result<()> {
        variable.replace_with(&value);
        self.replace_with(&in_term);
        Ok(())
    }

    async fn unify_known(x: &mut Self, y: &mut Self) -> Result<()> {
        // TODO: Find a way to not borrow `x` and `y` across await for
        // `VariableBinding::unify`
        #![allow(clippy::await_holding_refcell_ref)]
        let mut x_ref = x.value();
        let mut y_ref = y.value();

        // TODO: Can we use mutable borrowing to do the occurs check for us?
        match (&mut *x_ref, &mut *y_ref) {
            (GenericTerm::Type, GenericTerm::Type) => {}
            (
                GenericTerm::Variable(Variable::Global { name, value }),
                GenericTerm::Variable(Variable::Global {
                    name: name1,
                    value: value1,
                }),
            ) if name == name1 => {
                clone!(mut value, mut value1);
                drop((x_ref, y_ref));
                Self::unify_recurse(&mut value, &mut value1).await?
            }
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
                clone!(
                    mut function,
                    mut function1,
                    mut argument,
                    mut argument1,
                    mut typ,
                    mut typ1
                );
                drop((x_ref, y_ref));
                Self::unify_recurse(&mut function, &mut function1).await?;
                Self::unify_recurse(&mut argument, &mut argument1).await?;
                Self::unify_recurse(&mut typ, &mut typ1).await?;
            }
            (
                GenericTerm::Let { value, binding },
                GenericTerm::Let {
                    value: value1,
                    binding: binding1,
                },
            ) => {
                Self::unify_recurse(value, value1).await?;
                VariableBinding::unify(binding, binding1).await?
            }
            (GenericTerm::Pi(binding0), GenericTerm::Pi(binding1))
                if binding0.evaluation == binding1.evaluation =>
            {
                VariableBinding::unify(binding0, binding1).await?
            }
            (GenericTerm::Lambda(binding0), GenericTerm::Lambda(binding1)) => {
                VariableBinding::unify(binding0, binding1).await?
            }
            // It's safer to explicitly ignore each variant
            (GenericTerm::Type, _rhs)
            | (GenericTerm::Apply { .. }, _rhs)
            | (GenericTerm::Variable { .. }, _rhs)
            | (GenericTerm::Unknown { .. }, _rhs)
            | (GenericTerm::Let { .. }, _rhs)
            | (GenericTerm::Pi(_), _rhs)
            | (GenericTerm::Lambda(_), _rhs) => Err(InferenceError)?,
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

    fn variable_name(&mut self) -> Option<&'src str> {
        let GenericTerm::Variable(Variable::Local { name, .. }) = &*self.value() else {
            panic!("Expected a variable")
        };

        *name
    }
}

impl fmt::Debug for Term<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.debug(f)
    }
}

pub enum Variable<'src> {
    Local {
        name: Option<&'src str>,
        fresh: Option<Term<'src>>,
        typ: Term<'src>,
    },
    Global {
        name: &'src str,
        value: Term<'src>,
    },
}

impl<'src> Variable<'src> {
    fn local(name: Option<&'src str>, typ: Term<'src>) -> Self {
        Self::Local {
            name,
            fresh: None,
            typ,
        }
    }

    fn typ(&self) -> Term<'src> {
        match self {
            Self::Local { typ, .. } => typ.clone(),
            Self::Global { value, .. } => value.typ(),
        }
    }
}

impl<'src> TermReference<'src> for Term<'src> {
    type Variable = Variable<'src>;

    fn is_known(&self) -> bool {
        match &*self.0.borrow() {
            TermEnum::Value { term, .. } => term.is_known(),
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

impl<'src> VariableBinding<Term<'src>> {
    pub fn variable_name(&mut self) -> Option<&'src str> {
        self.variable.variable_name()
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
        let GenericTerm::Variable(Variable::Local { name, fresh, typ }) =
            &mut *self.variable.value()
        else {
            return self.variable.fresh_variables();
        };

        let variable = Term::new(GenericTerm::Variable(Variable::local(
            *name,
            typ.fresh_variables(),
        )));

        assert!(fresh.is_none());
        *fresh = Some(variable.clone());
        variable
    }

    fn free_fresh_variable(&mut self) {
        if let GenericTerm::Variable(Variable::Local { fresh, .. }) = &mut *self.variable.value() {
            *fresh = None;
        }
    }

    async fn unify(binding0: &mut Self, binding1: &mut Self) -> Result<()> {
        if binding0.evaluation != binding1.evaluation {
            return Err(InferenceError);
        }

        Term::unify_recurse(&mut binding0.variable.typ(), &mut binding1.variable.typ()).await?;

        // Keep the name if we can
        if binding0.variable_name().is_some() {
            binding1.variable.replace_with(&binding0.variable);
        } else {
            binding0.variable.replace_with(&binding1.variable);
        }

        Term::unify_recurse(&mut binding0.in_term, &mut binding1.in_term).await
    }
}
