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
    pub parameters: Vec<TypeParameter<'src>>,
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
    pub parameters: Vec<Field<'src>>,
}

impl<'src> TypeConstructor<'src> {
    pub fn empty(name: Identifier<'src>) -> Self {
        Self {
            name,
            parameters: Vec::new(),
        }
    }
}

/// `name` or `(name : Kind)`
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TypeParameter<'src> {
    pub name: Identifier<'src>,
    pub kind: Option<TypeExpression<'src>>,
    pub parentheses: usize,
}

impl<'src> TypeParameter<'src> {
    pub fn new(name: Identifier<'src>, kind: Option<TypeExpression<'src>>) -> Self {
        Self {
            name,
            kind,
            parentheses: 0,
        }
    }

    pub fn parenthesized(self) -> Self {
        Self {
            parentheses: self.parentheses + 1,
            ..self
        }
    }
}

/// `(name : Type)` or `Type`
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Field<'src> {
    pub name: Option<Identifier<'src>>,
    pub typ: TypeExpression<'src>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expression<'src> {
    Variable(Identifier<'src>),
    BinaryOperator {
        name: Operator,
        left: Box<Self>,
        right: Box<Self>,
    },
    TypeAnnotation {
        expression: Box<Self>,
        type_expression: Box<TypeExpression<'src>>,
    },
    Parenthesized(Box<Self>),
}

impl<'src> Expression<'src> {
    pub fn apply(left: Self, right: Self) -> Self {
        Self::BinaryOperator {
            name: Operator::Apply,
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

    pub fn type_annotation(expression: Self, type_expression: TypeExpression<'src>) -> Self {
        Self::TypeAnnotation {
            expression: Box::new(expression),
            type_expression: Box::new(type_expression),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Constructor)]
pub struct TypeExpression<'src> {
    pub expression: Expression<'src>,
    pub where_clause: WhereClause<'src>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Operator {
    Apply,
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
pub struct WhereClause<'src>(pub Vec<Expression<'src>>);

#[derive(Clone, Debug, Eq, PartialEq, Constructor)]
pub struct Identifier<'src> {
    pub name: &'src str,
    pub span: Span,
}
