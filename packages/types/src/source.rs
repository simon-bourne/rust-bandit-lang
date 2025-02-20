use std::rc::Rc;

use super::pretty::{Document, Layout, Operator, Side};
use crate::{
    context::Context, inference, pretty::TypeAnnotated, Binder, GenericTerm, Pretty, Result,
    TermReference, VariableBinding, VariableValue,
};

#[derive(Clone)]
pub struct Term<'src>(Rc<TermEnum<'src>>);

impl<'src> TermReference<'src> for Term<'src> {
    type Variable = Option<&'src str>;
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
            TermEnum::TypeAnnotation { term, typ } => {
                TypeAnnotated::new(term, typ).to_document(parent, layout)
            }
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
        Self::binding(
            Binder::Let,
            Some(name),
            VariableValue::Known {
                value: variable_value,
            },
            in_term,
        )
    }

    pub fn pi_type(name: Option<&'src str>, binding_typ: Self, result_type: Self) -> Self {
        Self::binding(
            Binder::Pi,
            name,
            VariableValue::Unknown { typ: binding_typ },
            result_type,
        )
    }

    pub fn lambda(name: &'src str, binding_typ: Self, in_term: Self) -> Self {
        Self::binding(
            Binder::Lambda,
            Some(name),
            VariableValue::Unknown { typ: binding_typ },
            in_term,
        )
    }

    pub fn has_type(self, typ: Self) -> Self {
        Self::new(TermEnum::TypeAnnotation { term: self, typ })
    }

    pub fn variable(name: &'src str) -> Self {
        Self::value(GenericTerm::Variable(Some(name)))
    }

    pub fn link(&self, ctx: &mut Context<'src>) -> Result<inference::Term<'src>> {
        Ok(match self.0.as_ref() {
            TermEnum::Value { term } => term.link(ctx)?,
            TermEnum::TypeAnnotation { term, typ } => {
                let term = term.link(ctx)?;
                let typ = &mut typ.link(ctx)?;
                inference::Term::unify(&mut term.typ(), typ)?;
                term
            }
        })
    }

    fn new(term: TermEnum<'src>) -> Self {
        Self(Rc::new(term))
    }

    fn value(term: GenericTerm<'src, Self>) -> Self {
        Self::new(TermEnum::Value { term })
    }

    fn binding(
        binder: Binder,
        name: Option<&'src str>,
        variable_value: VariableValue<Self>,
        in_term: Self,
    ) -> Self {
        Self::value(GenericTerm::VariableBinding(VariableBinding {
            name,
            binder,
            variable_value,
            in_term,
        }))
    }
}

enum TermEnum<'src> {
    Value { term: GenericTerm<'src, Term<'src>> },
    TypeAnnotation { term: Term<'src>, typ: Term<'src> },
}

impl<'src> TermEnum<'src> {
    fn is_known(&self) -> bool {
        match self {
            Self::Value {
                term: GenericTerm::Variable(None),
            } => false,
            Self::Value { .. } => true,
            Self::TypeAnnotation { term, .. } => term.is_known(),
        }
    }

    fn typ(&self, new: impl FnOnce(GenericTerm<'src, Term<'src>>) -> Term<'src>) -> Term<'src> {
        match self {
            Self::Value { term } => term.typ(new, |_| Term::unknown()),
            Self::TypeAnnotation { typ, .. } => typ.clone(),
        }
    }
}

impl<'src> GenericTerm<'src, Term<'src>> {
    fn link(&self, ctx: &mut Context<'src>) -> Result<inference::Term<'src>> {
        let new = |term| inference::Term::new(0, term);
        Ok(match self {
            Self::TypeOfType => new(GenericTerm::TypeOfType),
            Self::Constant { name, typ } => new(GenericTerm::Constant {
                name,
                typ: typ.link(ctx)?,
            }),
            Self::Apply {
                function,
                argument,
                typ,
            } => new(GenericTerm::Apply {
                function: function.link(ctx)?,
                argument: argument.link(ctx)?,
                typ: typ.link(ctx)?,
            }),
            Self::Variable(Some(name)) => ctx.lookup(name)?,
            Self::Variable(None) => inference::Term::unknown_value(),
            Self::VariableBinding(binding) => new(GenericTerm::VariableBinding(binding.link(ctx)?)),
        })
    }
}

impl<'src> VariableBinding<'src, Term<'src>> {
    fn link(
        &self,
        ctx: &mut Context<'src>,
    ) -> Result<VariableBinding<'src, inference::Term<'src>>> {
        let variable_value = match &self.variable_value {
            VariableValue::Known { value } => {
                let value = value.link(ctx)?;
                if let Some(name) = self.name {
                    inference::Term::variable(name, value)
                } else {
                    value
                }
            }
            VariableValue::Unknown { typ } => inference::Term::unknown(self.name, typ.link(ctx)?),
        };

        let in_term = if let Some(name) = self.name {
            ctx.with_variable(name, variable_value.clone(), |ctx| self.in_term.link(ctx))?
        } else {
            self.in_term.link(ctx)
        }?;

        Ok(VariableBinding {
            name: self.name,
            binder: self.binder,
            variable_value,
            in_term,
        })
    }
}
