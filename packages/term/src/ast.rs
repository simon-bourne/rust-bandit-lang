use crate::{Evaluation, InferenceError, Result, VariableBinding, constraints::Constraints, typed};

mod context;
mod pretty;

pub use context::{Context, LocalVariables, Value};
use derive_more::Constructor;

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
    type_constructor: Constant<'src>,
    value_constructors: Vec<Constant<'src>>,
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

impl<'src> Definition<'src> {
    pub fn function(name: &'src str, typ: Option<Term<'src>>, value: Term<'src>) -> Self {
        Self::Function(Function { name, typ, value })
    }

    pub fn context(definitions: impl IntoIterator<Item = Self>) -> Context<'src> {
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

        Context::new(types, functions)
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

    pub fn desugar(
        &self,
        ctx: &Context<'src>,
        constraints: &mut Constraints<'src>,
    ) -> Result<typed::Term<'src>> {
        self.desugar_local(ctx, &mut LocalVariables::default(), constraints)
    }

    fn desugar_local(
        &self,
        ctx: &Context<'src>,
        variables: &mut LocalVariables<'src>,
        constraints: &mut Constraints<'src>,
    ) -> Result<typed::Term<'src>> {
        use typed::Term as Core;

        Ok(match self.0.as_ref() {
            TermEnum::Type => Core::type_of_type(),
            TermEnum::Apply {
                function,
                argument,
                evaluation,
            } => Core::apply(
                function.desugar_local(ctx, variables, constraints)?,
                argument.desugar_local(ctx, variables, constraints)?,
                *evaluation,
                constraints,
            ),
            TermEnum::Variable(name) => ctx.lookup(name, variables, constraints)?,
            TermEnum::Unknown => Core::unknown_value(),
            TermEnum::Let { value, binding } => Core::let_binding(
                value.desugar_local(ctx, variables, constraints)?,
                binding.desugar(ctx, variables, constraints)?,
                constraints,
            ),
            TermEnum::Pi(binding) => {
                Core::pi(binding.desugar(ctx, variables, constraints)?, constraints)
            }
            TermEnum::FunctionType(left, right) => Core::pi(
                VariableBinding {
                    variable: Core::variable(
                        None,
                        left.desugar_local(ctx, variables, constraints)?,
                    ),
                    in_term: right.desugar_local(ctx, variables, constraints)?,
                    evaluation: Evaluation::Dynamic,
                },
                constraints,
            ),
            TermEnum::Lambda(binding) => {
                Core::lambda(binding.desugar(ctx, variables, constraints)?)
            }
            TermEnum::HasType { term, typ } => term
                .desugar_local(ctx, variables, constraints)?
                .has_type(typ.desugar_local(ctx, variables, constraints)?, constraints),
        })
    }

    fn new(term: TermEnum<'src>) -> Self {
        Self(Box::new(term))
    }

    fn desugar_variable(
        &self,
        ctx: &Context<'src>,
        variables: &mut LocalVariables<'src>,
        constraints: &mut Constraints<'src>,
    ) -> Result<typed::Term<'src>> {
        Ok(match self.0.as_ref() {
            TermEnum::HasType { term, typ } => Ok(term
                .desugar_variable(ctx, variables, constraints)?
                .has_type(typ.desugar_local(ctx, variables, constraints)?, constraints))?,
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
        constraints: &mut Constraints<'src>,
    ) -> Result<VariableBinding<typed::Term<'src>>> {
        let variable = self
            .variable
            .desugar_variable(ctx, variables, constraints)?;
        let in_term = ctx.in_scope(variables, variable.clone(), |ctx, variables| {
            self.in_term.desugar_local(ctx, variables, constraints)
        })?;

        Ok(VariableBinding {
            variable,
            in_term,
            evaluation: self.evaluation,
        })
    }
}
