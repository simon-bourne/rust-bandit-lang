use std::rc::Rc;

use pretty::RcDoc;

use crate::{ExprRefVariants, Expression, ExpressionRef, Inferred, Stage};

pub trait Pretty {
    fn to_document(
        &self,
        parent: Option<(Operator, Side)>,
        type_annotations: TypeAnnotations,
    ) -> Document;

    fn type_annotatation(
        &self,
        term: Document,
        term_operator: Option<Operator>,
        parent: Option<(Operator, Side)>,
        type_annotations: TypeAnnotations,
    ) -> Document;

    fn to_pretty_string(&self, width: usize) -> String {
        self.to_document(None, TypeAnnotations::On)
            .pretty(width)
            .to_string()
    }
}

impl Pretty for Rc<Expression<'_, Inferred>> {
    fn to_document(
        &self,
        parent: Option<(Operator, Side)>,
        type_annotations: TypeAnnotations,
    ) -> Document {
        self.as_ref().to_document(parent, type_annotations)
    }

    fn type_annotatation(
        &self,
        term: Document,
        term_operator: Option<Operator>,
        parent: Option<(Operator, Side)>,
        type_annotations: TypeAnnotations,
    ) -> Document {
        self.as_ref()
            .type_annotatation(term, term_operator, parent, type_annotations)
    }
}

impl Pretty for ExpressionRef<'_> {
    fn to_document(
        &self,
        parent: Option<(Operator, Side)>,
        type_annotations: TypeAnnotations,
    ) -> Document {
        match &*self.0.borrow() {
            ExprRefVariants::Known { expression } => {
                expression.to_document(parent, type_annotations)
            }
            ExprRefVariants::Unknown { typ } => {
                typ.type_annotatation(Document::text("_"), None, parent, type_annotations)
            }
            ExprRefVariants::Link { target } => target.to_document(parent, type_annotations),
        }
    }

    fn type_annotatation(
        &self,
        term: Document,
        term_operator: Option<Operator>,
        parent: Option<(Operator, Side)>,
        type_annotations: TypeAnnotations,
    ) -> Document {
        match &*self.0.borrow() {
            ExprRefVariants::Known { expression } => {
                expression.type_annotatation(term, term_operator, parent, type_annotations)
            }
            ExprRefVariants::Unknown { .. } => term,
            ExprRefVariants::Link { target } => {
                target.type_annotatation(term, term_operator, parent, type_annotations)
            }
        }
    }
}

impl<'src, S: Stage<'src>> Pretty for Expression<'src, S> {
    fn to_document(
        &self,
        parent: Option<(Operator, Side)>,
        type_annotations: TypeAnnotations,
    ) -> Document {
        match self {
            Self::Type => Document::text("Type"),
            Self::Apply {
                function,
                argument,
                typ,
            } => {
                let op = Operator::Apply;

                typ.type_annotatation(
                    Document::concat([
                        function.to_document(Some((op, Side::Left)), type_annotations),
                        Document::space(),
                        argument.to_document(Some((op, Side::Right)), type_annotations),
                    ]),
                    Some(op),
                    parent,
                    type_annotations,
                )
            }
            Self::Let {
                variable_value,
                binding,
            } => parenthesize_if(
                parent.is_some(),
                [
                    Document::text("let"),
                    binding.variable_type.type_annotatation(
                        Document::as_string(&binding.name),
                        None,
                        None,
                        type_annotations,
                    ),
                    Document::text(" = "),
                    variable_value.to_document(None, type_annotations),
                    Document::text(" in "),
                    binding.in_expression.to_document(None, type_annotations),
                ],
            ),
            Self::FunctionType(binding) => {
                let variable_name = binding.name.to_string();

                if variable_name == "_" {
                    let op = Operator::Arrow;
                    disambiguate(
                        Some(op),
                        parent,
                        [
                            binding
                                .variable_type
                                .to_document(Some((op, Side::Left)), type_annotations),
                            Document::text(" → "),
                            binding
                                .in_expression
                                .to_document(Some((op, Side::Right)), type_annotations),
                        ],
                    )
                } else {
                    parenthesize_if(
                        parent.is_some(),
                        [
                            Document::text("∀"),
                            binding.variable_type.type_annotatation(
                                Document::text(variable_name),
                                None,
                                None,
                                type_annotations,
                            ),
                            Document::text(" ⇒ "),
                            binding.in_expression.to_document(None, type_annotations),
                        ],
                    )
                }
            }
            Self::Lambda(binding) => parenthesize_if(
                parent.is_some(),
                [
                    Document::text("\\"),
                    binding.variable_type.type_annotatation(
                        Document::as_string(&binding.name),
                        None,
                        None,
                        type_annotations,
                    ),
                    Document::text(" ⇒ "),
                    binding.in_expression.to_document(None, type_annotations),
                ],
            ),
            Self::Variable { index, typ } => {
                typ.type_annotatation(Document::as_string(index), None, parent, type_annotations)
            }
        }
    }

    fn type_annotatation(
        &self,
        term: Document,
        term_operator: Option<Operator>,
        parent: Option<(Operator, Side)>,
        type_annotations: TypeAnnotations,
    ) -> Document {
        if matches!(type_annotations, TypeAnnotations::Off) {
            return disambiguate(term_operator, parent, [term]);
        }

        let op = Operator::HasType;

        disambiguate(
            Some(Operator::HasType),
            parent,
            [
                disambiguate(term_operator, Some((op, Side::Left)), [term]),
                Document::text(" : "),
                self.to_document(Some((op, Side::Right)), TypeAnnotations::Off),
            ],
        )
    }
}

pub fn disambiguate(
    operator: Option<Operator>,
    parent: Option<(Operator, Side)>,
    docs: impl IntoIterator<Item = Document>,
) -> Document {
    parenthesize_if(operator.is_some_and(|op| op.parenthesize(parent)), docs)
}

fn parenthesize_if(condition: bool, docs: impl IntoIterator<Item = Document>) -> Document {
    if condition {
        parenthesize(docs)
    } else {
        Document::concat(docs)
    }
}

fn parenthesize(docs: impl IntoIterator<Item = Document>) -> Document {
    Document::concat([
        Document::text("("),
        Document::concat(docs),
        Document::text(")"),
    ])
}

#[derive(Copy, Clone)]
pub enum Operator {
    HasType,
    Arrow,
    Apply,
}

impl Operator {
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
            Self::HasType => 0,
            Self::Arrow => 1,
            Self::Apply => 2,
        }
    }

    fn associativity(self) -> Side {
        match self {
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

#[derive(Copy, Clone)]
pub enum TypeAnnotations {
    On,
    Off,
}
