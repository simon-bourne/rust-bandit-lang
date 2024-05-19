use derive_more::Constructor;

use crate::lex::Span;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AST<'src> {
    pub items: Vec<Item<'src>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Item<'src> {
    Data(DataDeclaration<'src>),
    Function(Function<'src>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Function<'src> {
    pub name: Identifier<'src>,
}

/// A `data` declaration
///
/// ```bandit
/// data MyType (a : Type -> Type) (b : Type) c where ...
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DataDeclaration<'src> {
    pub name: Identifier<'src>,
    pub parameters: Vec<Expression<'src>>,
    pub where_clause: WhereClause<'src>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expression<'src> {
    Variable(Identifier<'src>),
    BinaryOperator {
        name: OperatorName<'src>,
        left: Box<Expression<'src>>,
        right: Box<Expression<'src>>,
    },
    TypeAnnotation {
        expression: Box<Expression<'src>>,
        type_expression: Box<Expression<'src>>,
        where_clause: WhereClause<'src>,
    },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum OperatorName<'src> {
    Apply,
    Named(Operator<'src>),
}

#[derive(Clone, Debug, Eq, PartialEq, Constructor)]
pub struct WhereClause<'src>(pub Vec<Expression<'src>>);

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
