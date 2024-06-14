// TODO: Deny unused
#![allow(unused)]
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use bandit_parser::ast::{self, Identifier};

pub struct Program<'src, A: Annotation> {
    data: Vec<DataDeclaration<'src, A>>,
    values: Vec<Value<'src, A>>,
}

struct DataDeclaration<'src, A: Annotation> {
    name: ast::Identifier<'src>,
    parameters: Vec<Type<'src, A>>,
}

struct Value<'src, A: Annotation> {
    name: ast::Identifier<'src>,
    typ: Type<'src, A>,
}

pub trait Annotation {
    type Type<'src>;
}

struct Inference;

impl Annotation for Inference {
    type Type<'src> = Rc<RefCell<Option<Type<'src, Self>>>>;
}

struct Inferred;

impl Annotation for Inferred {
    type Type<'src> = Rc<Type<'src, Self>>;
}

struct Context<'src, A: Annotation> {
    types: HashMap<Identifier<'src>, A::Type<'src>>,
}

enum Type<'src, A: Annotation> {
    Base,
    /// `∀a b c. a -> b -> c` is a type `(a : Type) -> (b : Type) -> (c : Type)
    /// -> a -> b -> c`, where the 1st 3 arguments are inferred by the
    /// compiler.
    Quantified {
        inferred: Vec<TypeConstructor<'src, A::Type<'src>>>,
        explicit: Box<Self>,
    },
    Constructor(TypeConstructor<'src, A::Type<'src>>),
    Arrow {
        left: A::Type<'src>,
        right: A::Type<'src>,
    },
    Apply {
        left: A::Type<'src>,
        right: A::Type<'src>,
        typ: A::Type<'src>,
    },
}

struct TypeConstructor<'src, Type> {
    name: ast::Identifier<'src>,
    typ: Type,
}

#[cfg(test)]
mod tests {
    use super::*;

    // TODO: This sholuld be removed once we actually use the types
    // Check there are no cycles in the types
    #[test]
    fn build() {
        let _inferred = Type::<'static, Inferred>::Base;
        let _inference = Type::<'static, Inference>::Base;
    }
}
