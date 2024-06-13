// TODO: Deny unused
#![allow(unused)]
use std::{cell::RefCell, mem::size_of, rc::Rc};

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
    type Type<'src> = RefCell<InferenceVariable<Type<'src, Self>, TypeKey>>;
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
    Arrow(Box<Arrow<Self>>),
    Apply(Box<Apply<Self, A::Type<'src>>>),
}

struct TypeConstructor<'src, Type> {
    name: ast::Identifier<'src>,
    typ: Rc<Type>,
}

struct Arrow<Child> {
    left: Child,
    right: Child,
}

struct Apply<Child, Type> {
    left: Child,
    right: Child,
    typ: Type,
}
