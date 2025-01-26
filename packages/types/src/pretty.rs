use std::rc::Rc;

use crate::{
    Annotation, Document, ExprRefVariants, Expression, ExpressionRef, Inferred, Parentheses,
    TypeAnnotations, VariableIndex,
};

pub trait Pretty {
    fn to_document(&self, type_annotations: TypeAnnotations) -> Document;

    fn type_annotatation(
        &self,
        term: Document,
        parentheses: Parentheses,
        type_annotations: TypeAnnotations,
    ) -> Document;

    fn is_infix(&self) -> bool;

    fn to_pretty_string(&self, width: usize) -> String {
        self.to_document(TypeAnnotations::On)
            .pretty(width)
            .to_string()
    }
}

impl Pretty for Rc<Expression<'_, Inferred>> {
    fn to_document(&self, type_annotations: TypeAnnotations) -> Document {
        self.as_ref().to_document(type_annotations)
    }

    fn is_infix(&self) -> bool {
        self.as_ref().is_infix()
    }

    fn type_annotatation(
        &self,
        term: Document,
        parentheses: Parentheses,
        type_annotations: TypeAnnotations,
    ) -> Document {
        self.as_ref()
            .type_annotatation(term, parentheses, type_annotations)
    }
}

impl Pretty for ExpressionRef<'_> {
    fn to_document(&self, type_annotations: TypeAnnotations) -> Document {
        match &*self.0.borrow() {
            ExprRefVariants::Known { expression } => expression.to_document(type_annotations),
            ExprRefVariants::Unknown { typ } => {
                typ.type_annotatation(Document::text("_"), Parentheses::On, type_annotations)
            }
            ExprRefVariants::Link { target } => target.to_document(type_annotations),
        }
    }

    fn is_infix(&self) -> bool {
        match &*self.0.borrow() {
            ExprRefVariants::Known { expression } => expression.is_infix(),
            ExprRefVariants::Unknown { .. } => false,
            ExprRefVariants::Link { target } => target.is_infix(),
        }
    }

    fn type_annotatation(
        &self,
        term: Document,
        parentheses: Parentheses,
        type_annotations: TypeAnnotations,
    ) -> Document {
        match &*self.0.borrow() {
            ExprRefVariants::Known { expression } => {
                expression.type_annotatation(term, parentheses, type_annotations)
            }
            ExprRefVariants::Unknown { .. } => term,
            ExprRefVariants::Link { target } => {
                target.type_annotatation(term, parentheses, type_annotations)
            }
        }
    }
}

impl<'src, A: Annotation<'src>> Pretty for Expression<'src, A> {
    fn to_document(&self, type_annotations: TypeAnnotations) -> Document {
        match self {
            Self::Type => Document::text("Type"),
            Self::Apply {
                function,
                argument,
                typ,
            } => {
                let (left, right) = if function.is_infix() {
                    (argument, function)
                } else {
                    (function, argument)
                };

                typ.type_annotatation(
                    parenthesize([
                        left.to_document(type_annotations),
                        Document::space(),
                        right.to_document(type_annotations),
                    ]),
                    Parentheses::On,
                    type_annotations,
                )
            }
            Self::Let {
                variable_value,
                binding,
            } => parenthesize([
                Document::text("let"),
                binding.variable_type.type_annotatation(
                    Document::as_string(&binding.name),
                    Parentheses::Off,
                    type_annotations,
                ),
                Document::text(" = "),
                variable_value.to_document(type_annotations),
                Document::text(" in "),
                binding.in_expression.to_document(type_annotations),
            ]),
            Self::FunctionType(binding) => parenthesize([
                binding.variable_type.to_document(type_annotations),
                Document::text(" -> "),
                binding.in_expression.to_document(type_annotations),
            ]),
            Self::Lambda(binding) => parenthesize([
                Document::text("\\"),
                binding.variable_type.type_annotatation(
                    Document::as_string(&binding.name),
                    Parentheses::Off,
                    type_annotations,
                ),
                Document::text(" = "),
                binding.in_expression.to_document(type_annotations),
            ]),
            Self::Variable { index, typ } => typ.type_annotatation(
                Document::as_string(index),
                Parentheses::On,
                type_annotations,
            ),
        }
    }

    fn is_infix(&self) -> bool {
        match self {
            Expression::Variable { index, .. } => index.is_infix(),
            _ => false,
        }
    }

    fn type_annotatation(
        &self,
        term: Document,
        parentheses: Parentheses,
        type_annotations: TypeAnnotations,
    ) -> Document {
        if matches!(type_annotations, TypeAnnotations::Off) {
            return term;
        }

        let annotated = [
            term,
            Document::text(" : "),
            self.to_document(TypeAnnotations::Off),
        ];

        match parentheses {
            Parentheses::On => parenthesize(annotated),
            Parentheses::Off => Document::concat(annotated),
        }
    }
}

fn parenthesize(docs: impl IntoIterator<Item = Document>) -> Document {
    Document::concat([
        Document::text("("),
        Document::concat(docs),
        Document::text(")"),
    ])
}
