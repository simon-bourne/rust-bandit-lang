use crate::{Evaluation, InferenceError, Result, VariableBinding, context::ContextOwner, typed};

mod pretty;

use derive_more::Constructor;

use crate::context::{Context, LocalVariables, Value};

pub enum Definition<'src> {
    Function(Function<'src>),
    Data(Data<'src>),
}

#[derive(Constructor)]
pub struct Function<'src> {
    name: &'src str,
    typ: Option<Term<'src>>,
    value: Term<'src>,
}

pub struct Data<'src> {
    pub type_constructor: Constant<'src>,
    pub value_constructors: Vec<Constant<'src>>,
}

impl<'src> Data<'src> {
    pub fn new(
        name: &'src str,
        typ: Option<Term<'src>>,
        value_constructors: Vec<Constant<'src>>,
    ) -> Self {
        Self {
            type_constructor: Constant { name, typ },
            value_constructors,
        }
    }
}

#[derive(Constructor)]
pub struct Constant<'src> {
    name: &'src str,
    typ: Option<Term<'src>>,
}

impl<'src> Constant<'src> {
    pub fn name(&self) -> &'src str {
        self.name
    }

    pub fn desugar(&self, ctx: &Context<'src>) -> Result<typed::Term<'src>> {
        Ok(typed::Term::constant(
            self.name,
            if let Some(typ) = self.typ.as_ref() {
                typ.desugar(ctx)?
            } else {
                typed::Term::unknown_type()
            },
        ))
    }
}

impl<'src> Definition<'src> {
    pub fn function(name: &'src str, typ: Option<Term<'src>>, value: Term<'src>) -> Self {
        Self::Function(Function { name, typ, value })
    }

    pub fn context(definitions: impl IntoIterator<Item = Self>) -> ContextOwner<'src> {
        let mut functions = Vec::new();
        let mut types = Vec::new();

        for definition in definitions {
            match definition {
                Definition::Function(f) => {
                    let typ = f.typ.unwrap_or_else(Term::unknown);
                    let value = Value::with_type(f.value, typ);
                    functions.push((f.name, value));
                }
                Definition::Data(data) => {
                    types.push(data);
                }
            }
        }

        ContextOwner::new(types, functions)
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

    pub fn let_binding(variable: Self, value: Self, in_term: Self, evaluation: Evaluation) -> Self {
        Self::new(TermEnum::Let {
            value,
            binding: VariableBinding {
                variable,
                in_term,
                evaluation,
            },
        })
    }

    pub fn pi_type(variable: Self, in_term: Self) -> Self {
        Self::new(TermEnum::Pi(VariableBinding {
            variable,
            in_term,
            evaluation: Evaluation::Static,
        }))
    }

    pub fn function_type(input_type: Self, output_type: Self) -> Self {
        Self::new(TermEnum::FunctionType(input_type, output_type))
    }

    pub fn lambda(variable: Self, in_term: Self, evaluation: Evaluation) -> Self {
        Self::new(TermEnum::Lambda(VariableBinding {
            variable,
            in_term,
            evaluation,
        }))
    }

    pub fn has_type(self, typ: Self) -> Self {
        Self::new(TermEnum::HasType { term: self, typ })
    }

    pub fn variable(name: &'src str) -> Self {
        Self::new(TermEnum::Variable(name))
    }

    pub fn desugar(&self, ctx: &Context<'src>) -> Result<typed::Term<'src>> {
        self.desugar_local(ctx, &mut LocalVariables::default())
    }

    fn desugar_local(
        &self,
        ctx: &Context<'src>,
        variables: &mut LocalVariables<'src>,
    ) -> Result<typed::Term<'src>> {
        use typed::Term as Core;

        Ok(match self.0.as_ref() {
            TermEnum::Type => Core::type_of_type(),
            TermEnum::Apply {
                function,
                argument,
                evaluation,
            } => Core::apply(
                ctx,
                function.desugar_local(ctx, variables)?,
                argument.desugar_local(ctx, variables)?,
                *evaluation,
            ),
            TermEnum::Variable(name) => ctx.lookup(variables, name)?,
            TermEnum::Unknown => Core::unknown_value(),
            TermEnum::Let { value, binding } => Core::let_binding(
                ctx,
                value.desugar_local(ctx, variables)?,
                binding.desugar(ctx, variables)?,
            ),
            TermEnum::Pi(binding) => Core::pi(ctx, binding.desugar(ctx, variables)?),
            TermEnum::FunctionType(left, right) => Core::pi(
                ctx,
                VariableBinding {
                    variable: Core::variable(None, left.desugar_local(ctx, variables)?),
                    in_term: right.desugar_local(ctx, variables)?,
                    evaluation: Evaluation::Dynamic,
                },
            ),
            TermEnum::Lambda(binding) => Core::lambda(binding.desugar(ctx, variables)?),
            TermEnum::HasType { term, typ } => term
                .desugar_local(ctx, variables)?
                .has_type(ctx, typ.desugar_local(ctx, variables)?),
        })
    }

    fn new(term: TermEnum<'src>) -> Self {
        Self(Box::new(term))
    }

    fn desugar_variable(
        &self,
        ctx: &Context<'src>,
        variables: &mut LocalVariables<'src>,
    ) -> Result<typed::Term<'src>> {
        Ok(match self.0.as_ref() {
            TermEnum::HasType { term, typ } => Ok(term
                .desugar_variable(ctx, variables)?
                .has_type(ctx, typ.desugar_local(ctx, variables)?))?,
            TermEnum::Variable(name) => {
                typed::Term::variable(Some(name), typed::Term::unknown_type())
            }
            TermEnum::Unknown => typed::Term::variable(None, typed::Term::unknown_type()),
            _ => Err(InferenceError::InvalidVariable)?,
        })
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
    fn desugar(
        &self,
        ctx: &Context<'src>,
        variables: &mut LocalVariables<'src>,
    ) -> Result<VariableBinding<typed::Term<'src>>> {
        let variable = self.variable.desugar_variable(ctx, variables)?;
        let in_term = variables.in_scope(variable.clone(), |variables| {
            self.in_term.desugar_local(ctx, variables)
        })?;

        Ok(VariableBinding {
            variable,
            in_term,
            evaluation: self.evaluation,
        })
    }
}
