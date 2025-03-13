use std::rc::Rc;

use super::pretty::{Document, Layout, Operator, Side};
use crate::{
    GenericTerm, Pretty, Result, TermReference, VariableBinding, VariableValue,
    context::Context,
    linked::{self, VariableId},
    pretty::has_type,
};

#[derive(Clone)]
pub struct Term<'src>(Rc<TermEnum<'src>>);

impl<'src> TermReference<'src> for Term<'src> {
    type Type = Self;
    type VariableId = &'src str;
    type VariableReference = Option<&'src str>;
    type VariableValue = VariableValue<Self>;

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

    pub fn unknown() -> Self {
        Self::value(GenericTerm::Variable(None))
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
            typ: Self::unknown(),
        })
    }

    pub fn let_binding(name: &'src str, variable_value: Self, in_term: Self) -> Self {
        Self::value(GenericTerm::Let(VariableBinding {
            id: Some(name),
            variable_value: VariableValue::Known {
                value: variable_value,
            },
            in_term,
        }))
    }

    pub fn pi_type(name: Option<&'src str>, binding_typ: Self, result_type: Self) -> Self {
        Self::value(GenericTerm::Pi(VariableBinding {
            id: name,
            variable_value: VariableValue::Unknown { typ: binding_typ },
            in_term: result_type,
        }))
    }

    pub fn lambda(name: &'src str, binding_typ: Self, in_term: Self) -> Self {
        Self::value(GenericTerm::Lambda(VariableBinding {
            id: Some(name),
            variable_value: VariableValue::Unknown { typ: binding_typ },
            in_term,
        }))
    }

    pub fn has_type(self, typ: Self) -> Self {
        Self::new(TermEnum::HasType { term: self, typ })
    }

    pub fn variable(name: &'src str) -> Self {
        Self::value(GenericTerm::Variable(Some(name)))
    }

    pub fn link(&self, ctx: &mut Context<'src>) -> Result<linked::Term<'src>> {
        match self.0.as_ref() {
            TermEnum::Value { term } => term.link(ctx),
            TermEnum::HasType { term, typ } => term.link(ctx)?.has_type(typ.link(ctx)?),
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
            Self::Value {
                term: GenericTerm::Variable(None),
            } => false,
            Self::Value { .. } => true,
            Self::HasType { term, .. } => term.is_known(),
        }
    }

    fn typ(&self, new: impl FnOnce(GenericTerm<'src, Term<'src>>) -> Term<'src>) -> Term<'src> {
        match self {
            Self::Value { term } => term.typ(new, |_| Term::unknown()),
            Self::HasType { typ, .. } => typ.clone(),
        }
    }
}

impl<'src> GenericTerm<'src, Term<'src>> {
    fn link(&self, ctx: &mut Context<'src>) -> Result<linked::Term<'src>> {
        use linked::Term as Linked;

        Ok(match self {
            Self::TypeOfType => Linked::type_of_type(),
            Self::Constant { name, typ } => Linked::constant(name, typ.link(ctx)?)?,
            Self::Apply {
                function,
                argument,
                typ,
            } => Linked::apply(function.link(ctx)?, argument.link(ctx)?, typ.link(ctx)?)?,
            Self::Variable(Some(name)) => ctx.lookup(name)?,
            Self::Variable(None) => Linked::unknown_value(),
            Self::Let(binding) => Linked::let_binding(binding.link(ctx)?),
            Self::Pi(binding) => Linked::pi(binding.link(ctx)?),
            Self::Lambda(binding) => Linked::lambda(binding.link(ctx)?),
        })
    }
}

impl<'src> VariableBinding<'src, Term<'src>> {
    fn link(&self, ctx: &mut Context<'src>) -> Result<VariableBinding<'src, linked::Term<'src>>> {
        let name = self.id;
        let id = name.map(VariableId::new);
        let variable_value = match &self.variable_value {
            VariableValue::Known { value } => linked::Term::variable(id.clone(), value.link(ctx)?),
            VariableValue::Unknown { typ } => linked::Term::unknown(id.clone(), typ.link(ctx)?),
        };

        let in_term = ctx.in_scope(name, variable_value.clone(), |ctx| self.in_term.link(ctx))?;

        Ok(VariableBinding {
            id,
            variable_value,
            in_term,
        })
    }
}
