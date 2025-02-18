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
        self.0.typ(Self::value)
    }
}

impl Pretty for Term<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        match self.0.as_ref() {
            TermVariants::Value { term } => term.to_document(parent, layout),
            TermVariants::TypeAnnotation { term, typ } => {
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
        Self::value(GenericTerm::Variable(Some(name)))
    }

    pub fn link(&self, ctx: &mut Context<'src>) -> Result<inference::Term<'src>> {
        Ok(match self.0.as_ref() {
            TermVariants::Value { term } => term.link(ctx)?,
            TermVariants::TypeAnnotation { term, typ } => {
                let term = term.link(ctx)?;
                let typ = &mut typ.link(ctx)?;
                inference::Term::unify(&mut term.typ(), typ)?;
                term
            }
        })
    }

    fn new(term: TermVariants<'src>) -> Self {
        Self(Rc::new(term))
    }

    fn value(term: GenericTerm<'src, Self>) -> Self {
        Self::new(TermVariants::Value { term })
    }

    fn binding(
        binder: Binder,
        name: Option<&'src str>,
        variable_value: Self,
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

enum TermVariants<'src> {
    Value { term: GenericTerm<'src, Term<'src>> },
    TypeAnnotation { term: Term<'src>, typ: Term<'src> },
}

impl<'src> TermVariants<'src> {
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
        Ok(inference::Term::new(
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
                Self::Variable(None) => return Ok(inference::Term::unknown_value()),
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
