use pretty::RcDoc;

use super::{Expression, ExpressionReference, VariableBinding};
use crate::Variable;

pub trait Pretty {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotations: Annotation) -> Document;

    fn to_pretty_string(&self, width: usize) -> String {
        self.to_document(None, Annotation::On)
            .pretty(width)
            .to_string()
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

pub fn variable_to_document<'src>(
    name: &str,
    value: &impl ExpressionReference<'src>,
    parent: Option<(Operator, Side)>,
    annotation: Annotation,
) -> Document {
    if annotation == Annotation::Off {
        return Document::as_string(name);
    }

    let variable = |parent| {
        let typ = value.typ();

        if typ.is_known() {
            Operator::HasType.to_document(
                parent,
                |_parent| Document::as_string(name),
                &typ,
                Annotation::Off,
            )
        } else {
            Document::as_string(name)
        }
    };

    if value.is_known() {
        Operator::Equals.to_document(parent, variable, value, annotation)
    } else {
        variable(parent)
    }
}

impl<'src, Expr: ExpressionReference<'src>> Pretty for Expression<'src, Expr> {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotation: Annotation) -> Document {
        match self {
            Self::TypeOfType => Document::text("Type"),
            Self::Constant { name, typ } => {
                annotate_with_type(|_| Document::as_string(name), typ, parent, annotation)
            }
            Self::Apply {
                function,
                argument,
                typ,
            } => annotate_with_type(
                |parent| {
                    Operator::Apply.to_document(
                        parent,
                        |parent| function.to_document(parent, annotation),
                        argument,
                        annotation,
                    )
                },
                typ,
                parent,
                annotation,
            ),
            Self::Let(binding) => parenthesize_if(
                parent.is_some(),
                [
                    Document::text("let"),
                    Document::space(),
                    binding.to_document(annotation),
                ],
            ),
            Self::FunctionType(binding) => {
                let variable_name = binding.name;

                if variable_name != "_" {
                    parenthesize_if(
                        parent.is_some(),
                        [Document::text("∀"), binding.to_document(annotation)],
                    )
                } else {
                    Operator::Arrow.to_document(
                        parent,
                        |parent| {
                            binding
                                .variable_value
                                .typ()
                                .to_document(parent, Annotation::Off)
                        },
                        &binding.in_expression,
                        Annotation::Off,
                    )
                }
            }
            Self::Lambda(binding) => parenthesize_if(
                parent.is_some(),
                [Document::text("\\"), binding.to_document(annotation)],
            ),
            Self::Variable(var) => var.to_document(parent, annotation),
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

pub fn annotate_with_type(
    term: impl FnOnce(Option<(Operator, Side)>) -> Document,
    typ: &impl Pretty,
    parent: Option<(Operator, Side)>,
    annotation: Annotation,
) -> Document {
    if annotation == Annotation::Off {
        return term(parent);
    }

    Operator::HasType.to_document(parent, term, typ, Annotation::Off)
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
        left: impl FnOnce(Option<(Operator, Side)>) -> Document,
        right: &impl Pretty,
        right_annotation: Annotation,
    ) -> Document {
        parenthesize_if(
            self.parenthesize(parent),
            [
                left(Some((self, Side::Left))),
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
