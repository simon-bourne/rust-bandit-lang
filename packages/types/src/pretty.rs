use std::rc::Rc;

use crate::{Annotation, ExprRefVariants, Expression, ExpressionRef, Inferred, Pretty, PrettyDoc};

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

impl<'src, A: Annotation<'src>> Pretty for Expression<'src, A> {
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
                PrettyDoc::as_string(&binding.name),
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
                PrettyDoc::text("\\"),
                PrettyDoc::as_string(&binding.name),
                PrettyDoc::text(":"),
                binding.variable_type.pretty(),
                PrettyDoc::text(" = "),
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
