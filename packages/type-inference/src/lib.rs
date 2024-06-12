// TODO: Deny unused
#![allow(unused)]
use std::mem::size_of;

use bandit_parser::ast;
use slotmap::{new_key_type, DefaultKey, SlotMap};

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
    type Type<'src> = InferenceVariable<Type<'src, Self>, TypeKey>;
}

struct Inferred;

impl Annotation for Inferred {
    type Type<'src> = Type<'src, Self>;
}

struct Context<'src, A: Annotation> {
    types: SlotMap<TypeKey, A::Type<'src>>,
}

new_key_type! {struct TypeKey;}

enum InferenceVariable<Known, Unknown> {
    Known(Known),
    Unknown(Unknown),
}

enum TypeOperator {
    Arrow,
    Apply,
}

enum Type<'src, A: Annotation> {
    Base,
    /// `∀a b c. a -> b -> c` is a type `(a : Type) -> (b : Type) -> (c : Type)
    /// -> a -> b -> c`, where the 1st 3 arguments are inferred by the
    /// compiler.
    Quantified {
        inferred: Vec<Self>,
        explicit: Box<Self>,
    },
    Atom {
        name: ast::Identifier<'src>,
        typ: Box<A::Type<'src>>,
    },
    BinaryOperator {
        operator: TypeOperator,
        left: Box<Self>,
        right: Box<Self>,
        typ: Box<A::Type<'src>>,
    },
}
