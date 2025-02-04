use super::{ExprVariants, Expression};
use crate::{
    pretty::{Annotation, Document, Operator, Side, TypeAnnotated},
    Pretty,
};

impl Pretty for Expression<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotation: Annotation) -> Document {
        match &*self.0.borrow() {
            ExprVariants::Known { expression } => expression.to_document(parent, annotation),
            ExprVariants::Unknown { typ } => {
                TypeAnnotated::new("_", typ).to_document(parent, annotation)
            }
            ExprVariants::Link { target } => target.to_document(parent, annotation),
        }
    }
}
