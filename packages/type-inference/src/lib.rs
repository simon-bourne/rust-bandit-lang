// TODO: Deny unused
#![allow(unused)]
use std::collections::HashMap;

use bandit_parser::ast;
use slotmap::{new_key_type, DefaultKey, SlotMap};

pub struct Program<'src, A: Annotation<'src>> {
    data: Vec<DataDeclaration<'src, A>>,
    values: Vec<Value<'src, A>>,
}

struct DataDeclaration<'src, A: Annotation<'src>> {
    name: ast::Identifier<'src>,
    parameters: Vec<TypeParameter<'src, A>>,
}

struct Value<'src, A: Annotation<'src>> {
    name: ast::Identifier<'src>,
    typ: A::Type,
}

pub trait Annotation<'src> {
    type Kind;
    type Type;
    type TypeIdentifier;
    type KindIdentifier;
}

pub struct Source;

impl<'src> Annotation<'src> for Source {
    type Kind = Option<Kind<Self::KindIdentifier>>;
    type KindIdentifier = ();
    type Type = Option<QuantifiedType<'src, Self>>;
    type TypeIdentifier = ast::Identifier<'src>;
}

struct Inference;

impl<'src> Annotation<'src> for Inference {
    type Kind = Kind<Self::KindIdentifier>;
    type KindIdentifier = InferenceVariable<(), TypeInferenceVariable>;
    type Type = QuantifiedType<'src, Self>;
    type TypeIdentifier = InferenceVariable<ast::Identifier<'src>, TypeInferenceVariable>;
}

pub struct QuantifiedType<'src, A: Annotation<'src>> {
    parameters: Vec<TypeParameter<'src, A>>,
    typ: Type<A::TypeIdentifier, Self>,
}

struct TypeParameter<'src, A: Annotation<'src>> {
    name: A::TypeIdentifier,
    kind: A::Kind,
}

pub struct Kind<Id>(Type<Id, Self>);

struct Context<'src> {
    types: SlotMap<TypeInferenceVariable, QuantifiedType<'src, Inference>>,
    kinds: SlotMap<KindInferenceVariable, Kind<<Inference as Annotation<'src>>::KindIdentifier>>,
}

new_key_type! {struct TypeInferenceVariable;}
new_key_type! {struct KindInferenceVariable;}

enum InferenceVariable<Known, Unknown> {
    Known(Known),
    Unknown(Unknown),
}

enum Type<Identifier, Child> {
    Atom(Identifier),
    Function {
        parameter: Box<Child>,
        return_type: Box<Child>,
    },
}
