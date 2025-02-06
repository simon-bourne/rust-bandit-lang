use super::{ExprVariants, Expression, Names};
use crate::{
    pretty::{Annotation, Document, Operator, Side, TypeAnnotated},
    Pretty,
};

impl Pretty for Expression<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotation: Annotation) -> Document {
        match &*self.0.borrow() {
            ExprVariants::Known { expression, .. } => expression.to_document(parent, annotation),
            ExprVariants::Unknown { names, typ } => {
                TypeAnnotated::new(names, typ).to_document(parent, annotation)
            }
            ExprVariants::Link { target } => target.to_document(parent, annotation),
        }
    }
}

impl Pretty for Names<'_> {
    fn to_document(
        &self,
        _parent: Option<(crate::pretty::Operator, crate::pretty::Side)>,
        _annotations: crate::pretty::Annotation,
    ) -> crate::pretty::Document {
        // TODO: Print other aliases
        if let Some(name) = self.0.iter().copied().next() {
            Document::as_string(name)
        } else {
            Document::text("_")
        }
    }
}
