use std::rc::Rc;

use crate::{
    Annotation, Document, ExprRefVariants, Expression, ExpressionRef, Inferred, Pretty,
    VariableIndex,
};

impl Pretty for Rc<Expression<'_, Inferred>> {
    fn to_document(&self) -> Document {
        self.as_ref().to_document()
    }

    fn is_infix(&self) -> bool {
        self.as_ref().is_infix()
    }

    fn type_annotatation(&self, term: Document, parenthesized: bool) -> Document {
        self.as_ref().type_annotatation(term, parenthesized)
    }
}

impl Pretty for ExpressionRef<'_> {
    fn to_document(&self) -> Document {
        match &*self.0.borrow() {
            ExprRefVariants::Known { expression } => expression.to_document(),
            ExprRefVariants::Unknown => Document::text("_"),
            ExprRefVariants::Link { target } => target.to_document(),
        }
    }

    fn is_infix(&self) -> bool {
        match &*self.0.borrow() {
            ExprRefVariants::Known { expression } => expression.is_infix(),
            ExprRefVariants::Unknown => false,
            ExprRefVariants::Link { target } => target.is_infix(),
        }
    }

    fn type_annotatation(&self, term: Document, parenthesized: bool) -> Document {
        match &*self.0.borrow() {
            ExprRefVariants::Known { expression } => {
                expression.type_annotatation(term, parenthesized)
            }
            ExprRefVariants::Unknown => term,
            ExprRefVariants::Link { target } => target.type_annotatation(term, parenthesized),
        }
    }
}

impl<'src, A: Annotation<'src>> Pretty for Expression<'src, A> {
    fn to_document(&self) -> Document {
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
                    parenthesize([left.to_document(), Document::space(), right.to_document()]),
                    true,
                )
            }
            Self::Let {
                variable_value,
                binding,
            } => parenthesize([
                Document::text("let"),
                binding
                    .variable_type
                    .type_annotatation(Document::as_string(&binding.name), false),
                Document::text(" = "),
                variable_value.to_document(),
                Document::text(" in "),
                binding.in_expression.to_document(),
            ]),
            Self::FunctionType(binding) => parenthesize([
                binding.variable_type.to_document(),
                Document::text(" -> "),
                binding.in_expression.to_document(),
            ]),
            Self::Lambda(binding) => parenthesize([
                Document::text("\\"),
                binding
                    .variable_type
                    .type_annotatation(Document::as_string(&binding.name), false),
                Document::text(" = "),
                binding.in_expression.to_document(),
            ]),
            Self::Variable { index, typ } => {
                typ.type_annotatation(Document::as_string(index), true)
            }
        }
    }

    fn is_infix(&self) -> bool {
        match self {
            Expression::Variable { index, .. } => index.is_infix(),
            _ => false,
        }
    }

    fn type_annotatation(&self, term: Document, parenthesized: bool) -> Document {
        let annotated = [term, Document::text(":"), self.to_document()];

        if parenthesized {
            parenthesize(annotated)
        } else {
            Document::concat(annotated)
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
