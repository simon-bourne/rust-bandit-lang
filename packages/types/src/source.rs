use std::rc::Rc;

use derive_more::Constructor;

use crate::{
    Evaluation, GenericTerm, Result, TermReference, VariableBinding, context::Context, linked,
};

mod pretty;

#[derive(Constructor)]
pub struct FunctionDefinition<'src> {
    name: &'src str,
    typ: Option<Term<'src>>,
    value: Option<Term<'src>>,
}

impl<'src> FunctionDefinition<'src> {
    pub fn context(definitions: impl IntoIterator<Item = Self>) -> Context<'src> {
        Context::new(definitions.into_iter().map(|Self { name, typ, value }| {
            (
                name,
                value
                    .unwrap_or_else(Term::unknown_value)
                    .has_type(typ.unwrap_or_else(Term::unknown_type)),
            )
        }))
    }
}

#[derive(Clone)]
pub struct Term<'src>(Rc<TermEnum<'src>>);

impl<'src> TermReference<'src> for Term<'src> {
    type Variable = &'src str;

    fn is_known(&self) -> bool {
        self.0.is_known()
    }

    fn typ(&self) -> Self {
        self.0.typ(Self::value)
    }
}

impl<'src> Term<'src> {
    pub fn typ() -> Self {
        Self::value(GenericTerm::Type)
    }

    pub fn unknown(typ: Self) -> Self {
        Self::value(GenericTerm::Unknown { typ })
    }

    pub fn unknown_type() -> Self {
        Self::unknown(Self::typ())
    }

    pub fn unknown_value() -> Self {
        Self::unknown(Self::unknown_type())
    }

    pub fn apply(self, argument: Self) -> Self {
        Self::value(GenericTerm::Apply {
            function: self,
            argument,
            typ: Self::unknown_type(),
            evaluation: Evaluation::Dynamic,
        })
    }

    pub fn static_apply(self, argument: Self) -> Self {
        Self::value(GenericTerm::Apply {
            function: self,
            argument,
            typ: Self::unknown_type(),
            evaluation: Evaluation::Static,
        })
    }

    pub fn let_binding(name: &'src str, variable_value: Self, in_term: Self) -> Self {
        Self::value(GenericTerm::Let {
            value: variable_value.clone(),
            binding: VariableBinding {
                name: Some(name),
                variable: Self::bound_variable(variable_value.typ()),
                in_term,
                evaluation: Evaluation::Dynamic,
            },
        })
    }

    pub fn pi_type(
        name: Option<&'src str>,
        binding_typ: Self,
        result_type: Self,
        evaluation: Evaluation,
    ) -> Self {
        Self::value(GenericTerm::Pi(VariableBinding {
            name,
            variable: Self::bound_variable(binding_typ),
            in_term: result_type,
            evaluation,
        }))
    }

    pub fn lambda(name: &'src str, binding_typ: Self, in_term: Self) -> Self {
        Self::value(GenericTerm::Lambda(VariableBinding {
            name: Some(name),
            variable: Self::bound_variable(binding_typ),
            in_term,
            evaluation: Evaluation::Dynamic,
        }))
    }

    pub fn has_type(self, typ: Self) -> Self {
        Self::new(TermEnum::HasType { term: self, typ })
    }

    pub fn variable(name: &'src str) -> Self {
        Self::value(GenericTerm::Variable(name))
    }

    fn bound_variable(typ: Self) -> Self {
        Self::unknown(typ)
    }

    pub fn link(&self, ctx: &mut Context<'src>) -> Result<linked::Term<'src>> {
        match self.0.as_ref() {
            TermEnum::Value { term } => term.link(ctx),
            TermEnum::HasType { term, typ } => {
                Ok(term.link(ctx)?.has_type(typ.link(ctx)?, ctx.constraints()))
            }
        }
    }

    fn new(term: TermEnum<'src>) -> Self {
        Self(Rc::new(term))
    }

    fn value(term: GenericTerm<'src, Self>) -> Self {
        Self::new(TermEnum::Value { term })
    }
}

enum TermEnum<'src> {
    Value { term: GenericTerm<'src, Term<'src>> },
    HasType { term: Term<'src>, typ: Term<'src> },
}

impl<'src> TermEnum<'src> {
    fn is_known(&self) -> bool {
        match self {
            Self::Value { term } => term.is_known(),
            Self::HasType { term, .. } => term.is_known(),
        }
    }

    fn typ(&self, new: impl FnOnce(GenericTerm<'src, Term<'src>>) -> Term<'src>) -> Term<'src> {
        match self {
            Self::Value { term } => term.typ(new, |_| Term::unknown_value()),
            Self::HasType { typ, .. } => typ.clone(),
        }
    }
}

impl<'src> GenericTerm<'src, Term<'src>> {
    fn link(&self, ctx: &mut Context<'src>) -> Result<linked::Term<'src>> {
        use linked::Term as Linked;

        Ok(match self {
            Self::Type => Linked::typ(),
            Self::Apply {
                function,
                argument,
                typ,
                evaluation,
            } => Linked::apply(
                function.link(ctx)?,
                argument.link(ctx)?,
                typ.link(ctx)?,
                *evaluation,
                ctx.constraints(),
            ),
            Self::Variable(name) => ctx.lookup(name)?,
            Self::Unknown { typ } => Linked::unknown(typ.link(ctx)?),
            Self::Let { value, binding } => {
                Linked::let_binding(value.link(ctx)?, binding.link(ctx)?, ctx.constraints())
            }
            Self::Pi(binding) => Linked::pi(binding.link(ctx)?),
            Self::Lambda(binding) => Linked::lambda(binding.link(ctx)?),
        })
    }
}

impl<'src> VariableBinding<'src, Term<'src>> {
    fn link(&self, ctx: &mut Context<'src>) -> Result<VariableBinding<'src, linked::Term<'src>>> {
        let name = self.name;
        let variable = linked::Term::local_variable(name, self.binding_type().link(ctx)?);
        let in_term = ctx.in_scope(name, variable.clone(), |ctx| self.in_term.link(ctx))?;

        Ok(VariableBinding {
            name,
            variable,
            in_term,
            evaluation: self.evaluation,
        })
    }
}
