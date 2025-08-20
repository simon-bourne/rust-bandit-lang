use std::rc::Rc;

use super::pretty::{Document, Layout, Operator, Side};
use crate::{
    Evaluation, GenericTerm, Pretty, Result, TermReference, VariableBinding, context::Context,
    linked, pretty::has_type,
};

#[derive(Clone)]
pub struct Term<'src>(Rc<TermEnum<'src>>);

impl<'src> TermReference<'src> for Term<'src> {
    type VariableName = &'src str;

    fn is_known(&self) -> bool {
        self.0.is_known()
    }

    fn typ(&self) -> Self {
        self.0.typ(Self::value)
    }
}

impl Pretty for Term<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        match self.0.as_ref() {
            TermEnum::Value { term } => term.to_document(parent, layout),
            TermEnum::HasType { term, typ } => has_type(term, typ).to_document(parent, layout),
        }
    }
}

impl<'src> Term<'src> {
    pub fn type_of_type() -> Self {
        Self::value(GenericTerm::TypeOfType)
    }

    pub fn unknown(typ: Self) -> Self {
        Self::value(GenericTerm::Unknown { typ })
    }

    pub fn unknown_type() -> Self {
        Self::unknown(Self::type_of_type())
    }

    pub fn unknown_value() -> Self {
        Self::unknown(Self::unknown_type())
    }

    pub fn type_constant(name: &'src str) -> Self {
        Self::value(GenericTerm::Constant {
            name,
            typ: Self::type_of_type(),
        })
    }

    pub fn constant(name: &'src str, typ: Self) -> Self {
        Self::value(GenericTerm::Constant { name, typ })
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
        Self::value(GenericTerm::Variable {
            name,
            typ: Self::unknown_type(),
        })
    }

    fn bound_variable(typ: Self) -> Self {
        Self::unknown(typ)
    }

    pub fn link(&self, ctx: &mut Context<'src>) -> Result<linked::Term<'src>> {
        match self.0.as_ref() {
            TermEnum::Value { term } => term.link(ctx),
            TermEnum::HasType { term, typ } => term
                .link(ctx)?
                .has_type(typ.link(ctx)?, &mut ctx.constraints_mut()),
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
            Self::Value { term } => term.typ(new),
            Self::HasType { typ, .. } => typ.clone(),
        }
    }
}

impl<'src> GenericTerm<'src, Term<'src>> {
    fn link(&self, ctx: &mut Context<'src>) -> Result<linked::Term<'src>> {
        use linked::Term as Linked;

        Ok(match self {
            Self::TypeOfType => Linked::type_of_type(),
            Self::Constant { name, typ } => {
                Linked::constant(name, typ.link(ctx)?, &mut ctx.constraints_mut())?
            }
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
                &mut ctx.constraints_mut(),
            )?,
            Self::Variable { name, typ } => ctx
                .lookup(name)?
                .has_type(typ.link(ctx)?, &mut ctx.constraints_mut())?,
            Self::Unknown { typ } => Linked::unknown(typ.link(ctx)?),
            Self::Let { value, binding } => Linked::let_binding(
                value.link(ctx)?,
                binding.link(ctx)?,
                &mut ctx.constraints_mut(),
            )?,
            Self::Pi(binding) => Linked::pi(binding.link(ctx)?),
            Self::Lambda(binding) => Linked::lambda(binding.link(ctx)?),
        })
    }
}

impl<'src> VariableBinding<'src, Term<'src>> {
    fn link(&self, ctx: &mut Context<'src>) -> Result<VariableBinding<'src, linked::Term<'src>>> {
        let name = self.name;
        let variable = linked::Term::variable(name, self.binding_type().link(ctx)?);
        let in_term = ctx.in_scope(name, variable.clone(), |ctx| self.in_term.link(ctx))?;

        Ok(VariableBinding {
            name,
            variable,
            in_term,
            evaluation: self.evaluation,
        })
    }
}
