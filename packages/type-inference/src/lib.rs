// TODO: Deny unused
#![allow(unused)]
use bandit_parser::ast;
use slotmap::{new_key_type, DefaultKey, SlotMap};

pub struct Program<'src, A: Annotation> {
    data: Vec<DataDeclaration<'src, A>>,
    values: Vec<Value<'src, A>>,
}

struct DataDeclaration<'src, A: Annotation> {
    name: ast::Identifier<'src>,
    parameters: Vec<TypeParameter<'src, A>>,
}

struct Value<'src, A: Annotation> {
    name: ast::Identifier<'src>,
    typ: QuantifiedType<'src, A>,
}

pub trait Annotation {
    type KindId;
    type TypeId<'src>;
}

struct Inference;

impl Annotation for Inference {
    type KindId = InferenceVariable<(), KindKey>;
    type TypeId<'src> = InferenceVariable<ast::Identifier<'src>, TypeKey>;
}

struct Inferred;

impl Annotation for Inferred {
    type KindId = ();
    type TypeId<'src> = ast::Identifier<'src>;
}

pub struct QuantifiedType<'src, A: Annotation> {
    parameters: Vec<TypeParameter<'src, A>>,
    typ: Type<A::TypeId<'src>>,
}

struct TypeParameter<'src, A: Annotation> {
    name: ast::Identifier<'src>,
    kind: Type<A::KindId>,
}

struct Context<'src> {
    types: SlotMap<TypeKey, QuantifiedType<'src, Inference>>,
    kinds: SlotMap<KindKey, Type<<Inference as Annotation>::KindId>>,
}

new_key_type! {struct TypeKey;}
new_key_type! {struct KindKey;}

enum InferenceVariable<Known, Unknown> {
    Known(Known),
    Unknown(Unknown),
}

enum Type<Identifier> {
    Atom(Identifier),
    Function {
        parameter: Box<Self>,
        return_type: Box<Self>,
    },
}
