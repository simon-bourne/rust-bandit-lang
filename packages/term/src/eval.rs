use crate::{ArgumentStyle, Variable, VariableBinding, context::TermId, typed};

pub struct Term<'src> {
    id: TermId,
    term: Box<TermEnum<'src>>,
}

impl<'src> Term<'src> {
    fn new(id: TermId, term: TermEnum<'src>) -> Self {
        Self {
            id,
            term: Box::new(term),
        }
    }

    pub fn typ(id: TermId) -> Self {
        Self::new(id, TermEnum::Type)
    }

    pub fn apply(
        id: TermId,
        function: Self,
        argument: Self,
        typ: Self,
        argument_style: ArgumentStyle,
    ) -> Self {
        Self::new(
            id,
            TermEnum::Apply {
                function,
                argument,
                typ,
                argument_style,
            },
        )
    }

    pub fn bound_variable(
        id: TermId,
        name: Option<&'src str>,
        typ: Self,
        de_bruijn_index: usize,
    ) -> Self {
        Self::new(
            id,
            TermEnum::BoundVariable {
                name,
                typ,
                de_bruijn_index,
            },
        )
    }

    pub fn free_variable(id: TermId, variable: typed::Term<'src>) -> Self {
        Self::new(id, TermEnum::FreeVariable(variable))
    }

    pub fn constant(id: TermId, name: &'src str, typ: Self) -> Self {
        Self::new(id, TermEnum::Constant { name, typ })
    }

    pub fn let_binding(id: TermId, value: Self, in_term: Self) -> Self {
        Self::new(
            id,
            TermEnum::Let {
                value,
                binding: VariableBinding {
                    variable: (),
                    in_term,
                    discriminator: (),
                },
            },
        )
    }

    pub fn pi(id: TermId, in_term: Self, typ: Self, discriminator: ArgumentStyle) -> Self {
        Self::new(
            id,
            TermEnum::Pi {
                binding: VariableBinding {
                    variable: (),
                    in_term,
                    discriminator,
                },
                typ,
            },
        )
    }

    pub fn lambda(id: TermId, in_term: Self, typ: Self, discriminator: ArgumentStyle) -> Self {
        Self::new(
            id,
            TermEnum::Lambda {
                binding: VariableBinding {
                    variable: (),
                    in_term,
                    discriminator,
                },
                typ,
            },
        )
    }
}

impl<'src> Variable for Term<'src> {
    type Declaration = ();
}

enum TermEnum<'src> {
    Type,
    Apply {
        function: Term<'src>,
        argument: Term<'src>,
        typ: Term<'src>,
        argument_style: ArgumentStyle,
    },
    BoundVariable {
        name: Option<&'src str>,
        typ: Term<'src>,
        de_bruijn_index: usize,
    },
    FreeVariable(typed::Term<'src>),
    Constant {
        name: &'src str,
        typ: Term<'src>,
    },
    Let {
        value: Term<'src>,
        binding: VariableBinding<Term<'src>, ()>,
    },
    Pi {
        binding: VariableBinding<Term<'src>, ArgumentStyle>,
        typ: Term<'src>,
    },
    Lambda {
        binding: VariableBinding<Term<'src>, ArgumentStyle>,
        typ: Term<'src>,
    },
}
