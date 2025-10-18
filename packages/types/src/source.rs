use std::rc::Rc;

use derive_more::Constructor;

use crate::{Evaluation, InferenceError, Result, VariableBinding, context::Context, linked};

mod pretty;

#[derive(Constructor)]
pub struct FunctionDefinition<'src> {
    name: &'src str,
    typ: Option<Term<'src>>,
    value: Term<'src>,
}

impl<'src> FunctionDefinition<'src> {
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

#[derive(Clone)]
pub struct Term<'src>(Rc<TermEnum<'src>>);

impl<'src> Term<'src> {
    pub fn typ() -> Self {
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

    pub fn let_binding(variable: Self, value: Self, in_term: Self) -> Self {
        Self::new(TermEnum::Let {
            value,
            binding: VariableBinding {
                variable,
                in_term,
                evaluation: Evaluation::Dynamic,
            },
        })
    }

    pub fn pi_type(variable: Self, in_term: Self, evaluation: Evaluation) -> Self {
        Self::new(TermEnum::Pi(VariableBinding {
            variable,
            in_term,
            evaluation,
        }))
    }

    pub fn function_type(input_type: Self, output_type: Self) -> Self {
        Self::new(TermEnum::FunctionType(input_type, output_type))
    }

    pub fn lambda(variable: Self, in_term: Self) -> Self {
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

    pub fn link(&self, ctx: &mut Context<'src>) -> Result<linked::Term<'src>> {
        use linked::Term as Linked;

        Ok(match self.0.as_ref() {
            TermEnum::Type => Linked::typ(),
            TermEnum::Apply {
                function,
                argument,
                evaluation,
            } => Linked::apply(
                function.link(ctx)?,
                argument.link(ctx)?,
                Linked::unknown_type(),
                *evaluation,
                ctx.constraints(),
            ),
            TermEnum::Variable(name) => ctx.lookup(name)?,
            TermEnum::Unknown => Linked::unknown_value(),
            TermEnum::Let { value, binding } => {
                Linked::let_binding(value.link(ctx)?, binding.link(ctx)?, ctx.constraints())
            }
            TermEnum::Pi(binding) => Linked::pi(binding.link(ctx)?),
            TermEnum::FunctionType(left, right) => Linked::pi(VariableBinding {
                variable: linked::Term::local_variable(None, left.link(ctx)?),
                in_term: right.link(ctx)?,
                evaluation: Evaluation::Dynamic,
            }),
            TermEnum::Lambda(binding) => Linked::lambda(binding.link(ctx)?),
            TermEnum::HasType { term, typ } => {
                term.link(ctx)?.has_type(typ.link(ctx)?, ctx.constraints())
            }
        })
    }

    fn new(term: TermEnum<'src>) -> Self {
        Self(Rc::new(term))
    }

    fn variable_name(&self) -> Result<&'src str> {
        match self.0.as_ref() {
            TermEnum::Variable(name) => Ok(*name),
            TermEnum::HasType { term, .. } => term.variable_name(),
            _ => Err(InferenceError),
        }
    }
}

type Binding<'src> = VariableBinding<Term<'src>>;

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
        binding: Binding<'src>,
    },
    Pi(Binding<'src>),
    FunctionType(Term<'src>, Term<'src>),
    Lambda(Binding<'src>),
    HasType {
        term: Term<'src>,
        typ: Term<'src>,
    },
}

impl<'src> Binding<'src> {
    fn link(&self, ctx: &mut Context<'src>) -> Result<VariableBinding<linked::Term<'src>>> {
        let name = self.variable.variable_name()?;
        let variable = linked::Term::local_variable(Some(name), linked::Term::unknown_type());
        let in_term = ctx.in_scope(name, variable.clone(), |ctx| self.in_term.link(ctx))?;

        Ok(VariableBinding {
            variable,
            in_term,
            evaluation: self.evaluation,
        })
    }
}
