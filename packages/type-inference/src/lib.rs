// TODO: Deny unused
#![allow(unused)]
use bandit_parser::ast;

pub trait Annotation<'src> {
    type Kind;
    type Type;
    type TypeIdentifier;
}

pub struct Source;

impl<'src> Annotation<'src> for Source {
    type Kind = Option<Kind>;
    type Type = Option<QuantifiedType<'src, Self>>;
    type TypeIdentifier = ast::Identifier<'src>;
}

pub struct Context<'src, A: Annotation<'src>> {
    data: Vec<DataDeclaration<'src, A>>,
}

pub struct QuantifiedType<'src, A: Annotation<'src>> {
    parameters: Vec<TypeParameter<'src, A>>,
    typ: Type<A::TypeIdentifier, Self>,
}

pub struct Kind(Type<(), Self>);

struct DataDeclaration<'src, A: Annotation<'src>> {
    name: ast::Identifier<'src>,
    parameters: Vec<TypeParameter<'src, A>>,
}

struct TypeParameter<'src, A: Annotation<'src>> {
    name: A::TypeIdentifier,
    kind: A::Kind,
}

struct Value<'src, A: Annotation<'src>> {
    name: ast::Identifier<'src>,
    typ: A::Type,
}

enum Type<Identifier, Child> {
    Atom(Identifier),
    Function {
        parameter: Box<Child>,
        return_type: Box<Child>,
    },
}
