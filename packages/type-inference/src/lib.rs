// TODO: Deny unused
#![allow(unused)]
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use bandit_parser::ast::{self, Identifier};

pub struct Program<'src, A: Annotation<'src>> {
    data: Vec<DataDeclaration<'src, A>>,
    values: Vec<Value<'src, A>>,
}

struct DataDeclaration<'src, A: Annotation<'src>> {
    name: ast::Identifier<'src>,
    parameters: Vec<Type<'src, A>>,
}

struct Value<'src, A: Annotation<'src>> {
    name: ast::Identifier<'src>,
    typ: Type<'src, A>,
}

pub trait Annotation<'src> {
    type Type;
}

struct Inference;

impl<'src> Annotation<'src> for Inference {
    type Type = RefCell<Option<Type<'src, Self>>>;
}

struct Inferred;

impl<'src> Annotation<'src> for Inferred {
    type Type = Type<'src, Self>;
}

struct Context<'src> {
    types: HashMap<Identifier<'src>, <Inference as Annotation<'src>>::Type>,
}

enum Type<'src, A: Annotation<'src>> {
    Base,
    /// `∀a b c. a -> b -> c` is a type `(a : Type) -> (b : Type) -> (c : Type)
    /// -> a -> b -> c`, where the first 3 arguments are inferred by the
    /// compiler.
    Quantified {
        inferred: Vec<TypeConstructor<'src, A::Type>>,
        explicit: Box<Self>,
    },
    Constructor(TypeConstructor<'src, A::Type>),
    Arrow(Rc<Arrow<A::Type>>),
    Apply(Rc<Apply<A::Type>>),
}

type InferenceType<'src> = <Inference as Annotation<'src>>::Type;

impl<'src> Type<'src, Inference> {
    fn unify(x: &mut Rc<InferenceType<'src>>, y: &mut Rc<InferenceType<'src>>) -> Result<(), ()> {
        if Rc::ptr_eq(x, y) {
            return Ok(());
        }

        if x.borrow().is_none() {
            *x = y.clone();
            return Ok(());
        }

        if y.borrow().is_none() {
            *y = x.clone();
            return Ok(());
        }

        todo!()
    }
}

struct Arrow<Type> {
    left: Type,
    right: Type,
}

struct Apply<Type> {
    left: Type,
    right: Type,
    typ: Type,
}

struct TypeConstructor<'src, Type> {
    name: ast::Identifier<'src>,
    typ: Rc<Type>,
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
