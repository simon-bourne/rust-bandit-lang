use super::{ExprRefVariants, InferenceExpression, VariableReference};
use crate::{
    pretty::{annotate_with_type, variable_to_document, Annotation, Document, Operator, Side},
    Pretty,
};

impl Pretty for InferenceExpression<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotation: Annotation) -> Document {
        match &*self.0.borrow() {
            ExprRefVariants::Known { expression } => expression.to_document(parent, annotation),
            ExprRefVariants::Unknown { typ } => {
                annotate_with_type(|_| Document::text("_"), typ, parent, annotation)
            }
            ExprRefVariants::Link { target } => target.to_document(parent, annotation),
        }
    }
}

impl Pretty for VariableReference<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotation: Annotation) -> Document {
        variable_to_document(self.name, &self.value, parent, annotation)
    }
}
