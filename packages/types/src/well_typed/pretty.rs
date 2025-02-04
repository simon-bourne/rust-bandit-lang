use super::Expression;
use crate::{
    pretty::{Annotation, Document, Operator, Side},
    Pretty,
};

impl Pretty for Expression<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotation: Annotation) -> Document {
        self.0.borrow().to_document(parent, annotation)
    }
}
