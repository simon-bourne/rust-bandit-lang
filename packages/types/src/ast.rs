use derive_more::Constructor;

use crate::{Evaluation, Result, core};

mod context;
mod pretty;

pub use context::Context;

#[derive(Constructor)]
pub struct Constant<'src> {
    name: &'src str,
    typ: Option<Term<'src>>,
    value: Term<'src>,
}

impl<'src> Constant<'src> {
    pub fn context(definitions: impl IntoIterator<Item = Self>) -> Context<'src> {
        Context::new(definitions.into_iter().map(|Self { name, typ, value }| {
            let value = if let Some(typ) = typ {
                value.has_type(typ)
            } else {
                value
            };

            (name, value)
        }))
    }
}

pub struct Term<'src>(Box<TermEnum<'src>>);

impl<'src> Term<'src> {
    pub fn type_of_type() -> Self {
        Self::new(TermEnum::Type)
    }

    pub fn unknown() -> Self {
        Self::new(TermEnum::Unknown)
    }

    pub fn apply(self, argument: Self) -> Self {
        Self::new(TermEnum::Apply {
            function: self,
            argument,
            evaluation: Evaluation::Dynamic,
        })
    }

    pub fn static_apply(self, argument: Self) -> Self {
        Self::new(TermEnum::Apply {
            function: self,
            argument,
            evaluation: Evaluation::Static,
        })
    }

    pub fn let_binding(variable: Variable<'src>, value: Self, in_term: Self) -> Self {
        Self::new(TermEnum::Let {
            value,
            binding: VariableBinding {
                variable,
                in_term,
                evaluation: Evaluation::Dynamic,
            },
        })
    }

    pub fn pi_type(variable: Variable<'src>, in_term: Self, evaluation: Evaluation) -> Self {
        Self::new(TermEnum::Pi(VariableBinding {
            variable,
            in_term,
            evaluation,
        }))
    }

    pub fn function_type(input_type: Self, output_type: Self) -> Self {
        Self::new(TermEnum::FunctionType(input_type, output_type))
    }

    pub fn lambda(variable: Variable<'src>, in_term: Self) -> Self {
        Self::new(TermEnum::Lambda(VariableBinding {
            variable,
            in_term,
            evaluation: Evaluation::Dynamic,
        }))
    }

    pub fn has_type(self, typ: Self) -> Self {
        Self::new(TermEnum::HasType { term: self, typ })
    }

    pub fn variable(name: &'src str) -> Self {
        Self::new(TermEnum::Variable(name))
    }

    pub fn desugar(&self, ctx: &mut Context<'src>) -> Result<core::Term<'src>> {
        use core::Term as Core;

        Ok(match self.0.as_ref() {
            TermEnum::Type => Core::type_of_type(),
            TermEnum::Apply {
                function,
                argument,
                evaluation,
            } => Core::apply(function.desugar(ctx)?, argument.desugar(ctx)?, *evaluation),
            TermEnum::Variable(name) => ctx.lookup(name)?,
            TermEnum::Unknown => Core::unknown(),
            TermEnum::Let { value, binding } => {
                Core::let_binding(value.desugar(ctx)?, binding.desugar(ctx)?)
            }
            TermEnum::Pi(binding) => Core::pi(binding.desugar(ctx)?),
            TermEnum::FunctionType(left, right) => Core::pi(core::VariableBinding::new(
                left.desugar(ctx)?,
                right.desugar(ctx)?,
                Evaluation::Dynamic,
            )),
            TermEnum::Lambda(binding) => Core::lambda(binding.desugar(ctx)?),
            TermEnum::HasType { term, typ } => term.desugar(ctx)?.has_type(typ.desugar(ctx)?),
        })
    }

    fn new(term: TermEnum<'src>) -> Self {
        Self(Box::new(term))
    }
}

enum TermEnum<'src> {
    Type,
    Apply {
        function: Term<'src>,
        argument: Term<'src>,
        evaluation: Evaluation,
    },
    Variable(&'src str),
    Unknown,
    Let {
        value: Term<'src>,
        binding: VariableBinding<'src>,
    },
    Pi(VariableBinding<'src>),
    FunctionType(Term<'src>, Term<'src>),
    Lambda(VariableBinding<'src>),
    HasType {
        term: Term<'src>,
        typ: Term<'src>,
    },
}

struct VariableBinding<'src> {
    variable: Variable<'src>,
    in_term: Term<'src>,
    evaluation: Evaluation,
}

impl<'src> VariableBinding<'src> {
    fn desugar(&self, ctx: &mut Context<'src>) -> Result<core::VariableBinding<'src>> {
        let in_term = ctx.in_scope(self.variable.name, |ctx| self.in_term.desugar(ctx))?;

        let typ = if let Some(typ) = self.variable.typ.as_ref() {
            typ.desugar(ctx)?
        } else {
            core::Term::unknown()
        };

        Ok(core::VariableBinding::new(typ, in_term, self.evaluation))
    }
}

struct Variable<'src> {
    name: Option<&'src str>,
    typ: Option<Term<'src>>,
}
