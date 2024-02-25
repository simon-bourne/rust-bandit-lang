use derive_more::Constructor;

use crate::lex::Span;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AST<'src> {
    pub items: Vec<Item<'src>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Item<'src> {
    Function(Function<'src>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Function<'src> {
    pub name: Identifier<'src>,
}

#[derive(Clone, Debug, Eq, PartialEq, Constructor)]
pub struct Identifier<'src> {
    pub name: &'src str,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq, Constructor)]
pub struct Operator<'src> {
    pub name: &'src str,
    span: Span,
}
