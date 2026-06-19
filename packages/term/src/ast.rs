use std::{
    cmp::{max, min},
    fmt,
    ops::Range,
};

use crate::{
    ArgumentStyle::{self, Explicit, Implicit},
    Pretty, Result, Variable, VariableBinding,
    context::{ContextOwner, TermId},
    typed,
};

mod pretty;

use derive_more::Constructor;

use crate::context::{Context, LocalVariables, Value};

pub enum Definition<'src> {
    Function(Function<'src>),
    Data(Data<'src>),
}

#[derive(Constructor)]
pub struct Function<'src> {
    source: Source,
    name: &'src str,
    typ: Option<Term<'src>>,
    value: Term<'src>,
}

pub struct Data<'src> {
    pub type_constructor: Declaration<'src>,
    pub value_constructors: Vec<Declaration<'src>>,
}

impl<'src> Data<'src> {
    pub fn new(
        type_constructor: Declaration<'src>,
        value_constructors: Vec<Declaration<'src>>,
    ) -> Self {
        Self {
            type_constructor,
            value_constructors,
        }
    }
}

#[derive(Constructor)]
pub struct Declaration<'src> {
    source: Source,
    name: &'src str,
    typ: Option<Term<'src>>,
}

impl<'src> Declaration<'src> {
    pub fn name(&self) -> &'src str {
        self.name
    }

    pub fn desugar_constant(&self, ctx: &Context<'src>) -> Result<typed::Term<'src>> {
        let id = TermId::source(ctx, self.source);
        let typ = if let Some(typ) = self.typ.as_ref() {
            typ.desugar(ctx)?
        } else {
            typed::Term::unknown_type(ctx, id.typ(ctx))
        };
        typed::Term::constant(ctx, id, self.name, typ)
    }

    fn desugar_variable(
        &self,
        ctx: &Context<'src>,
        variables: &mut LocalVariables<'src>,
    ) -> Result<typed::Term<'src>> {
        let id = TermId::source(ctx, self.source);
        let typ = if let Some(typ) = &self.typ {
            typ.desugar_local(ctx, variables, Explicit)?
        } else {
            typed::Term::unknown_type(ctx, id.typ(ctx))
        };

        typed::Term::variable(ctx, id, Some(self.name), typ)
    }
}

impl<'src> Definition<'src> {
    pub fn function(
        source: Source,
        name: &'src str,
        typ: Option<Term<'src>>,
        value: Term<'src>,
    ) -> Self {
        Self::Function(Function {
            source,
            name,
            typ,
            value,
        })
    }

    pub fn context(definitions: impl IntoIterator<Item = Self>) -> ContextOwner<'src> {
        let mut functions = Vec::new();
        let mut types = Vec::new();

        for definition in definitions {
            match definition {
                Definition::Function(f) => {
                    let typ = f.typ.unwrap_or_else(|| Term::unknown(f.source));
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

#[derive(Copy, Clone)]
pub struct Source {
    begin: usize,
    end: usize,
}

impl Source {
    pub fn begin() -> Self {
        Self { begin: 0, end: 0 }
    }

    pub fn combine(x: Self, y: Self) -> Self {
        Self {
            begin: min(x.begin, y.begin),
            end: max(x.end, y.end),
        }
    }
}

impl From<Source> for Range<usize> {
    fn from(value: Source) -> Self {
        value.begin..value.end
    }
}

impl From<Range<usize>> for Source {
    fn from(value: Range<usize>) -> Self {
        Self {
            begin: value.start,
            end: value.end,
        }
    }
}

pub struct Term<'src> {
    source: Source,
    value: Box<TermEnum<'src>>,
}

impl<'src> Term<'src> {
    pub fn type_of_type(source: Source) -> Self {
        Self::new(source, TermEnum::Type)
    }

    pub fn unknown(source: Source) -> Self {
        Self::new(source, TermEnum::Unknown)
    }

    pub fn apply(self, argument: Self) -> Self {
        Self::new(
            Source::combine(self.source, argument.source),
            TermEnum::Apply {
                function: self,
                argument,
            },
        )
    }

    pub fn let_binding(
        source: Source,
        variable: Declaration<'src>,
        value: Self,
        in_term: Self,
    ) -> Self {
        Self::new(
            source,
            TermEnum::Let {
                value,
                binding: VariableBinding {
                    variable,
                    in_term,
                    discriminator: (),
                },
            },
        )
    }

    pub fn pi_type(
        source: Source,
        variable: Declaration<'src>,
        in_term: Self,
        discriminator: ArgumentStyle,
    ) -> Self {
        Self::new(
            source,
            TermEnum::Pi(VariableBinding {
                variable,
                in_term,
                discriminator,
            }),
        )
    }

    pub fn function_type(
        input_type: Self,
        output_type: Self,
        argument_style: ArgumentStyle,
    ) -> Self {
        Self::new(
            Source::combine(input_type.source, output_type.source),
            TermEnum::FunctionType(input_type, output_type, argument_style),
        )
    }

    pub fn lambda(
        source: Source,
        variable: Declaration<'src>,
        in_term: Self,
        discriminator: ArgumentStyle,
    ) -> Self {
        Self::new(
            source,
            TermEnum::Lambda(VariableBinding {
                variable,
                in_term,
                discriminator,
            }),
        )
    }

    pub fn has_type(self, typ: Self) -> Self {
        Self::new(
            Source::combine(self.source, typ.source),
            TermEnum::HasType { term: self, typ },
        )
    }

    pub fn variable(source: Source, name: &'src str) -> Self {
        Self::new(source, TermEnum::Variable(name))
    }

    pub fn specify_implicits(source: Source, term: Self) -> Self {
        Self::new(source, TermEnum::SpecifyImplicits(term))
    }

    pub fn desugar(&self, ctx: &Context<'src>) -> Result<typed::Term<'src>> {
        self.desugar_local(ctx, &mut LocalVariables::default(), Explicit)
    }

    fn desugar_local(
        &self,
        ctx: &Context<'src>,
        variables: &mut LocalVariables<'src>,
        arg_style: ArgumentStyle,
    ) -> Result<typed::Term<'src>> {
        use typed::Term as Core;
        let id = TermId::source(ctx, self.source);

        Ok(match self.value.as_ref() {
            TermEnum::Type => Core::type_of_type(id),
            TermEnum::Apply { function, argument } => Core::apply(
                ctx,
                id.clone(),
                function.desugar_local(ctx, variables, arg_style)?,
                argument.desugar_local(ctx, variables, Explicit)?,
                Core::unknown_type(ctx, id),
                arg_style,
            ),
            TermEnum::Variable(name) => ctx.lookup(variables, name, arg_style)?,
            TermEnum::Unknown => Core::unknown_value(ctx, id),
            TermEnum::Let { value, binding } => Core::let_binding(
                ctx,
                id.clone(),
                value.desugar_local(ctx, variables, Explicit)?,
                binding.desugar(ctx, variables)?,
            )?,
            TermEnum::Pi(binding) => Core::pi(ctx, id, binding.desugar(ctx, variables)?)?,
            TermEnum::FunctionType(left, right, discriminator) => {
                let left = left.desugar_local(ctx, variables, Explicit)?;
                let variable =
                    Core::variable(ctx, TermId::anonymous_variable(&left.id(), ctx), None, left)?;
                let in_term = right.desugar_local(ctx, variables, Explicit)?;

                Core::pi(
                    ctx,
                    id,
                    VariableBinding {
                        variable,
                        in_term,
                        discriminator: *discriminator,
                    },
                )?
            }
            TermEnum::Lambda(binding) => Core::lambda(id, binding.desugar(ctx, variables)?),
            TermEnum::HasType { term, typ } => term
                .desugar_local(ctx, variables, arg_style)?
                .has_type(ctx, typ.desugar_local(ctx, variables, Explicit)?)?,
            TermEnum::SpecifyImplicits(term) => term.desugar_local(ctx, variables, Implicit)?,
        })
    }

    fn new(source: Source, term: TermEnum<'src>) -> Self {
        Self {
            source,
            value: Box::new(term),
        }
    }
}

impl fmt::Debug for Term<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.debug(f)
    }
}

enum TermEnum<'src> {
    Type,
    Apply {
        function: Term<'src>,
        argument: Term<'src>,
    },
    Variable(&'src str),
    Unknown,
    Let {
        value: Term<'src>,
        binding: VariableBinding<Term<'src>, ()>,
    },
    Pi(VariableBinding<Term<'src>, ArgumentStyle>),
    FunctionType(Term<'src>, Term<'src>, ArgumentStyle),
    Lambda(VariableBinding<Term<'src>, ArgumentStyle>),
    HasType {
        term: Term<'src>,
        typ: Term<'src>,
    },
    SpecifyImplicits(Term<'src>),
}

impl<'src> Variable for Term<'src> {
    type Declaration = Declaration<'src>;
}

impl<'src, Discriminator: Clone> VariableBinding<Term<'src>, Discriminator> {
    fn desugar(
        &self,
        ctx: &Context<'src>,
        variables: &mut LocalVariables<'src>,
    ) -> Result<VariableBinding<typed::Term<'src>, Discriminator>> {
        let variable = self.variable.desugar_variable(ctx, variables)?;
        let in_term = variables.in_scope(variable.clone(), |variables| {
            self.in_term.desugar_local(ctx, variables, Explicit)
        })?;

        Ok(VariableBinding {
            variable,
            in_term,
            discriminator: self.discriminator.clone(),
        })
    }
}
