use std::rc::Rc;

use super::{Annotation, ExprRefVariants, Expression, ExpressionRef, Inferred, Pretty, PrettyDoc};
use crate::SourceExpression;

impl Pretty for SourceExpression<'_> {
    fn pretty(&self) -> PrettyDoc {
        match self.0.as_ref() {
            Some(expr) => expr.pretty(),
            None => PrettyDoc::text("{unknown}"),
        }
    }
}

impl Pretty for Rc<Expression<'_, Inferred>> {
    fn pretty(&self) -> PrettyDoc {
        self.as_ref().pretty()
    }
}

impl Pretty for ExpressionRef<'_> {
    fn pretty(&self) -> PrettyDoc {
        match &*self.0.borrow() {
            ExprRefVariants::Known(owned) => owned.pretty(),
            ExprRefVariants::Unknown => PrettyDoc::text("{unknown}"),
            ExprRefVariants::Link(linked) => linked.pretty(),
        }
    }
}

impl<'src, A: Annotation<'src>> Expression<'src, A> {
    fn pretty(&self) -> PrettyDoc {
        match self {
            Self::Type => PrettyDoc::text("Type"),
            Self::Apply {
                function: left,
                argument: right,
                typ,
            } => PrettyDoc::concat([
                PrettyDoc::text("("),
                PrettyDoc::text("("),
                left.pretty(),
                PrettyDoc::space(),
                right.pretty(),
                PrettyDoc::text(")"),
                PrettyDoc::text(":"),
                typ.pretty(),
                PrettyDoc::text(")"),
            ]),
            Self::Let {
                variable_value,
                binding,
            } => PrettyDoc::concat([
                PrettyDoc::text("("),
                PrettyDoc::text("let"),
                PrettyDoc::text("_"),
                PrettyDoc::text(":"),
                binding.variable_type.pretty(),
                PrettyDoc::text(" = "),
                variable_value.pretty(),
                PrettyDoc::text(" in "),
                binding.in_expression.pretty(),
                PrettyDoc::text(")"),
            ]),
            Self::FunctionType(binding) => PrettyDoc::concat([
                PrettyDoc::text("("),
                binding.variable_type.pretty(),
                PrettyDoc::text(" -> "),
                binding.in_expression.pretty(),
                PrettyDoc::text(")"),
            ]),
            Self::Lambda(binding) => PrettyDoc::concat([
                PrettyDoc::text("("),
                PrettyDoc::text("_"),
                PrettyDoc::text(":"),
                binding.variable_type.pretty(),
                PrettyDoc::text(" -> "),
                binding.in_expression.pretty(),
                PrettyDoc::text(")"),
            ]),
            Self::Variable { index, typ } => PrettyDoc::concat([
                PrettyDoc::text("("),
                PrettyDoc::as_string(index),
                PrettyDoc::text(":"),
                typ.pretty(),
                PrettyDoc::text(")"),
            ]),
        }
    }
}
