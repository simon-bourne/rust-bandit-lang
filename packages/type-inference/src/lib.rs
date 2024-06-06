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
    type Kind;
    type Type<'src>;
}

struct Inference;

impl Annotation for Inference {
    type Kind = InferenceVariable<Kind<Self>, KindKey>;
    type Type<'src> = InferenceVariable<QuantifiedType<'src, Self>, TypeKey>;
}

struct Inferred;

impl Annotation for Inferred {
    type Kind = Kind<Self>;
    type Type<'src> = Type<'src, QuantifiedType<'src, Self>>;
}

pub struct QuantifiedType<'src, A: Annotation> {
    parameters: Vec<TypeParameter<'src, A>>,
    typ: Type<'src, A::Type<'src>>,
}

struct TypeParameter<'src, A: Annotation> {
    name: ast::Identifier<'src>,
    kind: A::Kind,
}

struct Context<'src, A: Annotation> {
    types: SlotMap<TypeKey, A::Type<'src>>,
    kinds: SlotMap<KindKey, A::Kind>,
}

new_key_type! {struct TypeKey;}
new_key_type! {struct KindKey;}

enum InferenceVariable<Known, Unknown> {
    Known(Known),
    Unknown(Unknown),
}

type Type<'src, Child> = UniverseType<ast::Identifier<'src>, Child>;
struct Kind<A: Annotation>(UniverseType<(), A::Kind>);

enum UniverseType<Identifier, Child> {
    Atom(Identifier),
    BinaryOperator {
        operator: TypeOperator,
        left: Box<Child>,
        right: Box<Child>,
    },
}

enum TypeOperator {
    Arrow,
    Apply,
}
