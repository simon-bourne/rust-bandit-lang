use pretty::RcDoc;

use super::{ExpressionReference, GenericExpression, VariableBinding};
use crate::{Variable, VariableReference};

pub trait Pretty {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotations: Annotation) -> Document;

    fn to_pretty_string(&self, width: usize) -> String {
        self.to_document(None, Annotation::On)
            .pretty(width)
            .to_string()
    }
}

impl<T: Pretty> Pretty for &'_ T {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotations: Annotation) -> Document {
        (*self).to_document(parent, annotations)
    }
}

impl Pretty for Variable<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotation: Annotation) -> Document {
        self.name.to_document(parent, annotation)
    }
}

impl<'src, Expr: ExpressionReference<'src>> VariableBinding<'src, Expr> {
    fn to_document(&self, annotation: Annotation) -> Document {
        Document::concat([
            variable_to_document(self.name, &self.variable_value, None, annotation),
            Document::text(" ⇒ "),
            self.in_expression.to_document(None, annotation),
        ])
    }
}

pub struct TypeAnnotated<Value: Pretty, Type: Pretty> {
    value: Value,
    typ: Option<Type>,
}

impl<'a, 'src, Value, Type> TypeAnnotated<Value, &'a Type>
where
    Value: Pretty,
    Type: ExpressionReference<'src> + 'a,
{
    pub fn new(value: Value, typ: &'a Type) -> Self {
        Self {
            value,
            typ: typ.is_known().then_some(typ),
        }
    }
}

impl<Value: Pretty, Type: Pretty> Pretty for TypeAnnotated<Value, Type> {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotations: Annotation) -> Document {
        if annotations == Annotation::On {
            if let Some(typ) = self.typ.as_ref() {
                return Operator::HasType.to_document(
                    parent,
                    &self.value,
                    &typ,
                    Annotation::On,
                    Annotation::Off,
                );
            }
        }
        self.value.to_document(parent, annotations)
    }
}

pub fn variable_to_document<'src>(
    name: &str,
    value: &impl ExpressionReference<'src>,
    parent: Option<(Operator, Side)>,
    annotation: Annotation,
) -> Document {
    let typ = &value.typ();
    let type_annotated = TypeAnnotated::new(name, typ);

    if value.is_known() {
        Operator::Equals.to_document(parent, &type_annotated, value, annotation, Annotation::Off)
    } else {
        type_annotated.to_document(parent, annotation)
    }
}

impl<'src, Expr: ExpressionReference<'src>> Pretty for GenericExpression<'src, Expr> {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotation: Annotation) -> Document {
        match self {
            Self::TypeOfType => Document::text("Type"),
            Self::Constant { name, typ } => {
                TypeAnnotated::new(*name, typ).to_document(parent, annotation)
            }
            Self::Apply {
                function,
                argument,
                typ,
            } => TypeAnnotated::new(BinaryOperator(function, Operator::Apply, argument), typ)
                .to_document(parent, annotation),
            Self::Let(binding) => parenthesize_if(
                parent.is_some(),
                [
                    Document::text("let"),
                    Document::space(),
                    binding.to_document(annotation),
                ],
            ),
            Self::Pi(binding) => {
                let variable_name = binding.name;

                if variable_name != "_" {
                    parenthesize_if(
                        parent.is_some(),
                        [Document::text("∀"), binding.to_document(annotation)],
                    )
                } else {
                    Operator::Arrow.to_document(
                        parent,
                        &binding.variable_value.typ(),
                        &binding.in_expression,
                        Annotation::Off,
                        Annotation::Off,
                    )
                }
            }
            Self::Lambda(binding) => parenthesize_if(
                parent.is_some(),
                [Document::text("\\"), binding.to_document(annotation)],
            ),
        }
    }
}

impl Pretty for str {
    fn to_document(&self, _parent: Option<(Operator, Side)>, _annotation: Annotation) -> Document {
        Document::as_string(self)
    }
}

impl Pretty for &str {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotation: Annotation) -> Document {
        (*self).to_document(parent, annotation)
    }
}

impl<'src, Value: ExpressionReference<'src>> Pretty for VariableReference<'src, Value> {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotation: Annotation) -> Document {
        variable_to_document(self.name, &self.value, parent, annotation)
    }
}

fn parenthesize_if(condition: bool, docs: impl IntoIterator<Item = Document>) -> Document {
    if condition {
        Document::concat([
            Document::text("("),
            Document::concat(docs),
            Document::text(")"),
        ])
    } else {
        Document::concat(docs)
    }
}

pub struct BinaryOperator<Left: Pretty, Right: Pretty>(pub Left, pub Operator, pub Right);

impl<Left: Pretty, Right: Pretty> Pretty for BinaryOperator<Left, Right> {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotations: Annotation) -> Document {
        self.1
            .to_document(parent, &self.0, &self.2, annotations, annotations)
    }
}
#[derive(Copy, Clone)]
pub enum Operator {
    Equals,
    HasType,
    Arrow,
    Apply,
}

impl Operator {
    pub fn to_document(
        self,
        parent: Option<(Operator, Side)>,
        left: &impl Pretty,
        right: &impl Pretty,
        left_annotation: Annotation,
        right_annotation: Annotation,
    ) -> Document {
        parenthesize_if(
            self.parenthesize(parent),
            [
                left.to_document(Some((self, Side::Left)), left_annotation),
                Document::text(match self {
                    Self::Equals => " = ",
                    Self::HasType => " : ",
                    Self::Arrow => " → ",
                    Self::Apply => " ",
                }),
                right.to_document(Some((self, Side::Right)), right_annotation),
            ],
        )
    }

    fn parenthesize(self, parent: Option<(Operator, Side)>) -> bool {
        let Some((parent, side)) = parent else {
            return false;
        };

        if self.precedence() > parent.precedence() {
            return false;
        }

        if self.precedence() < parent.precedence() {
            return true;
        }

        assert!(self.precedence() == parent.precedence());

        // We shouldn't have operators of the same precedence but different
        // associativity. Just in case:
        if self.associativity() != parent.associativity() {
            return true;
        }

        if self.associativity() == side {
            return false;
        }

        true
    }

    fn precedence(self) -> usize {
        match self {
            Self::Equals => 0,
            Self::HasType => 1,
            Self::Arrow => 2,
            Self::Apply => 3,
        }
    }

    fn associativity(self) -> Side {
        match self {
            Self::Equals => Side::Right,
            Self::HasType => Side::Right,
            Self::Arrow => Side::Right,
            Self::Apply => Side::Left,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Side {
    Left,
    Right,
}

pub type Document = RcDoc<'static>;

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Annotation {
    On,
    Off,
}
