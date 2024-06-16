use derive_more::Constructor;

use crate::lex::{NamedOperator, Span};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AST<'src> {
    pub items: Vec<Item<'src>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Item<'src> {
    Data(Data<'src>),
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

#[derive(Clone, Debug, Eq, PartialEq, Constructor)]
pub struct Data<'src> {
    pub declaration: DataDeclaration<'src>,
    pub constructors: VisibilityItems<TypeConstructor<'src>>,
}

#[derive(Clone, Debug, Eq, PartialEq, Constructor)]
pub struct TypeConstructor<'src> {
    pub name: Identifier<'src>,
    pub parameters: Option<Expression<'src>>,
}

impl<'src> TypeConstructor<'src> {
    pub fn empty(name: Identifier<'src>) -> Self {
        Self {
            name,
            parameters: None,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expression<'src> {
    Variable(Identifier<'src>),
    BinaryOperator {
        name: Operator,
        left: Box<Self>,
        right: Box<Self>,
    },
    Where {
        expression: Box<Self>,
        constraints: Box<Self>,
    },
    Parenthesized(Box<Self>),
    // TODO: Add abstraction (quantification/lambdas)
}

impl<'src> Expression<'src> {
    pub fn apply(left: Self, right: Self) -> Self {
        Self::BinaryOperator {
            name: Operator::Apply,
            left: Box::new(left),
            right: Box::new(right),
        }
    }

    pub fn line_separator(left: Self, right: Self) -> Self {
        Self::BinaryOperator {
            name: Operator::LineSeparator,
            left: Box::new(left),
            right: Box::new(right),
        }
    }

    pub fn binary_operator(left: Self, name: Operator, right: Self) -> Self {
        Self::BinaryOperator {
            name,
            left: Box::new(left),
            right: Box::new(right),
        }
    }

    pub fn parenthesized(self) -> Self {
        Self::Parenthesized(Box::new(self))
    }

    pub fn where_clause(self, constraints: Self) -> Self {
        Self::Where {
            expression: Box::new(self),
            constraints: Box::new(constraints),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Operator {
    Apply,
    LineSeparator,
    Named { name: NamedOperator, span: Span },
}

#[derive(Clone, Debug, Eq, PartialEq, Constructor)]
pub struct VisibilityItems<T> {
    pub visibility: Visibility,
    pub items: Vec<T>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Clone, Debug, Eq, PartialEq, Constructor)]
pub struct WhereClause<'src>(pub Option<Expression<'src>>);

#[derive(Clone, Debug, Hash, Eq, PartialEq, Constructor)]
pub struct Identifier<'src> {
    pub name: &'src str,
    pub span: Span,
}
