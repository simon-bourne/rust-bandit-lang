use std::rc::Rc;

use super::pretty::{Document, Layout, Operator, Side};
use crate::{
    context::Context, inference, pretty::TypeAnnotated, Binder, GenericTerm, Pretty, Result,
    TermReference, VariableBinding,
};

#[derive(Clone)]
pub struct Term<'src>(Rc<TermVariants<'src>>);

impl<'src> TermReference<'src> for Term<'src> {
    type Variable = Option<&'src str>;

    fn is_known(&self) -> bool {
        self.0.is_known()
    }

    fn typ(&self) -> Self {
        self.0.typ(Self::known)
    }
}

impl Pretty for Term<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        match self.0.as_ref() {
            TermVariants::Known { term } => term.to_document(parent, layout),
            TermVariants::Variable(variable) => variable.to_document(parent, layout),
            TermVariants::TypeAnnotation { term, typ } => {
                TypeAnnotated::new(term, typ).to_document(parent, layout)
            }
            TermVariants::Unknown { typ } => {
                TypeAnnotated::new(None, typ).to_document(parent, layout)
            }
        }
    }
}

impl<'src> Term<'src> {
    pub fn type_of_type() -> Self {
        Self::known(GenericTerm::TypeOfType)
    }

    pub fn unknown_value() -> Self {
        Self::new(TermVariants::Unknown {
            typ: Self::unknown_type(),
        })
    }

    pub fn unknown_type() -> Self {
        Self::new(TermVariants::Unknown {
            typ: Self::type_of_type(),
        })
    }

    pub fn type_constant(name: &'src str) -> Self {
        Self::known(GenericTerm::Constant {
            name,
            typ: Self::type_of_type(),
        })
    }

    pub fn constant(name: &'src str, typ: Self) -> Self {
        Self::known(GenericTerm::Constant { name, typ })
    }

    pub fn apply(self, argument: Self) -> Self {
        Self::known(GenericTerm::Apply {
            function: self,
            argument,
            typ: Self::unknown_type(),
        })
    }

    pub fn let_binding(name: &'src str, variable_value: Self, in_term: Self) -> Self {
        Self::binding(Binder::Let, Some(name), variable_value, in_term)
    }

    pub fn pi_type(name: Option<&'src str>, variable_value: Self, result_type: Self) -> Self {
        Self::binding(Binder::Pi, name, variable_value, result_type)
    }

    pub fn lambda(name: &'src str, variable_value: Self, in_term: Self) -> Self {
        Self::binding(Binder::Lambda, Some(name), variable_value, in_term)
    }

    pub fn has_type(self, typ: Self) -> Self {
        Self::new(TermVariants::TypeAnnotation { term: self, typ })
    }

    pub fn variable(name: &'src str) -> Self {
        Self::new(TermVariants::Variable(name))
    }

    pub fn link(&self, ctx: &mut Context<'src>) -> Result<inference::Term<'src>> {
        Ok(match self.0.as_ref() {
            TermVariants::Known { term } => term.link(ctx)?,
            TermVariants::Variable(name) => ctx.lookup(name)?,
            TermVariants::TypeAnnotation { term, typ } => {
                let term = term.link(ctx)?;
                let typ = &mut typ.link(ctx)?;
                inference::Term::unify(&mut term.typ(), typ)?;
                term
            }
            TermVariants::Unknown { typ } => inference::Term::unknown(typ.link(ctx)?),
        })
    }

    fn new(term: TermVariants<'src>) -> Self {
        Self(Rc::new(term))
    }

    fn known(term: GenericTerm<'src, Self>) -> Self {
        Self::new(TermVariants::Known { term })
    }

    fn binding(
        binder: Binder,
        name: Option<&'src str>,
        variable_value: Self,
        in_term: Self,
    ) -> Self {
        Self::known(GenericTerm::VariableBinding(VariableBinding {
            name,
            binder,
            variable_value,
            in_term,
        }))
    }
}

enum TermVariants<'src> {
    Known { term: GenericTerm<'src, Term<'src>> },
    TypeAnnotation { term: Term<'src>, typ: Term<'src> },
    Unknown { typ: Term<'src> },
    Variable(&'src str),
}

impl<'src> TermVariants<'src> {
    fn is_known(&self) -> bool {
        match self {
            Self::Known { .. } | Self::Variable(_) => true,
            Self::TypeAnnotation { term, .. } => term.is_known(),
            Self::Unknown { .. } => false,
        }
    }

    fn typ(&self, new: impl FnOnce(GenericTerm<'src, Term<'src>>) -> Term<'src>) -> Term<'src> {
        match self {
            Self::Known { term } => term.typ(new, |_| Term::unknown_type()),
            Self::Variable(_) => Term::unknown_type(),
            Self::TypeAnnotation { typ, .. } | Self::Unknown { typ, .. } => typ.clone(),
        }
    }
}

impl<'src> GenericTerm<'src, Term<'src>> {
    fn link(&self, ctx: &mut Context<'src>) -> Result<inference::Term<'src>> {
        Ok(inference::Term::new_known(
            0,
            match self {
                Self::TypeOfType => GenericTerm::TypeOfType,
                Self::Constant { name, typ } => GenericTerm::Constant {
                    name,
                    typ: typ.link(ctx)?,
                },
                Self::Apply {
                    function,
                    argument,
                    typ,
                } => GenericTerm::Apply {
                    function: function.link(ctx)?,
                    argument: argument.link(ctx)?,
                    typ: typ.link(ctx)?,
                },
                Self::Variable(Some(name)) => return ctx.lookup(name),
                Self::Variable(None) => return Ok(inference::Term::unknown_type()),
                Self::VariableBinding(binding) => GenericTerm::VariableBinding(binding.link(ctx)?),
            },
        ))
    }
}

impl<'src> VariableBinding<'src, Term<'src>> {
    fn link(
        &self,
        ctx: &mut Context<'src>,
    ) -> Result<VariableBinding<'src, inference::Term<'src>>> {
        let variable_value = self.variable_value.link(ctx)?;

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
