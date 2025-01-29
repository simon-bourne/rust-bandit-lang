use std::fmt;

use pretty::RcDoc;

use crate::{
    context::{Variable, VariableReference},
    ExprRefVariants, Expression, ExpressionRef, Stage, VariableBinding,
};

pub trait Pretty {
    fn to_document(
        &self,
        parent: Option<(Operator, Side)>,
        type_annotations: TypeAnnotations,
    ) -> Document;

    fn type_annotation(
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

impl fmt::Debug for ExpressionRef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.to_pretty_string(80))
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
                typ.type_annotation(Document::text("_"), None, parent, type_annotations)
            }
            ExprRefVariants::Link { target } => target.to_document(parent, type_annotations),
        }
    }

    fn type_annotation(
        &self,
        term: Document,
        term_operator: Option<Operator>,
        parent: Option<(Operator, Side)>,
        type_annotations: TypeAnnotations,
    ) -> Document {
        match &*self.0.borrow() {
            ExprRefVariants::Known { expression } => {
                expression.type_annotation(term, term_operator, parent, type_annotations)
            }
            ExprRefVariants::Unknown { .. } => term,
            ExprRefVariants::Link { target } => {
                target.type_annotation(term, term_operator, parent, type_annotations)
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
        let binding_doc = |binding: &VariableBinding<'src, S>| {
            Document::concat([
                Document::text("{"),
                Document::as_string(binding.name),
                Document::text(" = "),
                binding
                    .variable_value
                    .to_document(None, TypeAnnotations::On),
                Document::text("}"),
            ])
        };
        match self {
            Self::Literal(literal) => Document::as_string(literal),
            Self::Apply {
                function,
                argument,
                typ,
            } => {
                let op = Operator::Apply;

                typ.type_annotation(
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
            Self::Let(binding) => parenthesize_if(
                parent.is_some(),
                [
                    Document::text("let"),
                    Document::space(),
                    binding_doc(binding),
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
                            binding_doc(binding),
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
                            binding_doc(binding),
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
                    binding_doc(binding),
                    Document::text(" ⇒ "),
                    binding.in_expression.to_document(None, type_annotations),
                ],
            ),
            Self::Variable(var) => var.to_document(parent, type_annotations),
        }
    }

    fn type_annotation(
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
            Some(op),
            parent,
            [
                disambiguate(term_operator, Some((op, Side::Left)), [term]),
                Document::text(" : "),
                self.to_document(Some((op, Side::Right)), TypeAnnotations::Off),
            ],
        )
    }
}

impl fmt::Debug for VariableReference<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.to_pretty_string(80))
    }
}

impl Pretty for VariableReference<'_> {
    fn to_document(
        &self,
        parent: Option<(Operator, Side)>,
        type_annotations: TypeAnnotations,
    ) -> Document {
        if self.value.is_known() {
            self.value.to_document(parent, type_annotations)
        } else {
            let term = self.name().to_document(parent, type_annotations);
            self.value
                .typ()
                .type_annotation(term, None, parent, type_annotations)
        }
    }

    fn type_annotation(
        &self,
        term: Document,
        term_operator: Option<Operator>,
        parent: Option<(Operator, Side)>,
        type_annotations: TypeAnnotations,
    ) -> Document {
        self.name()
            .type_annotation(term, term_operator, parent, type_annotations)
    }
}

impl fmt::Debug for Variable<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.to_pretty_string(80))
    }
}

impl Pretty for Variable<'_> {
    fn to_document(
        &self,
        parent: Option<(crate::pretty::Operator, crate::pretty::Side)>,
        type_annotations: crate::pretty::TypeAnnotations,
    ) -> crate::pretty::Document {
        self.name().to_document(parent, type_annotations)
    }

    fn type_annotation(
        &self,
        term: crate::pretty::Document,
        term_operator: Option<crate::pretty::Operator>,
        parent: Option<(crate::pretty::Operator, crate::pretty::Side)>,
        type_annotations: crate::pretty::TypeAnnotations,
    ) -> crate::pretty::Document {
        self.name()
            .type_annotation(term, term_operator, parent, type_annotations)
    }
}

impl Pretty for &'_ str {
    fn to_document(
        &self,
        _parent: Option<(crate::pretty::Operator, crate::pretty::Side)>,
        _type_annotations: crate::pretty::TypeAnnotations,
    ) -> crate::pretty::Document {
        Document::as_string(self)
    }

    fn type_annotation(
        &self,
        term: crate::pretty::Document,
        term_operator: Option<crate::pretty::Operator>,
        parent: Option<(crate::pretty::Operator, crate::pretty::Side)>,
        type_annotations: crate::pretty::TypeAnnotations,
    ) -> crate::pretty::Document {
        if matches!(type_annotations, TypeAnnotations::Off) {
            return disambiguate(term_operator, parent, [term]);
        }

        let op = Operator::HasType;

        disambiguate(
            Some(op),
            parent,
            [
                disambiguate(term_operator, Some((op, Side::Left)), [term]),
                Document::text(" : "),
                Document::as_string(self),
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
