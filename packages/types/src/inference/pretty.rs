use super::{ExprVariants, Expression};
use crate::{
    pretty::{Document, Layout, Operator, Side, TypeAnnotated},
    Pretty,
};

impl Pretty for Expression<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        match &*self.0.borrow() {
            ExprVariants::Known { expression, .. } => expression.to_document(parent, layout),
            ExprVariants::Unknown { name, typ } => {
                let doc = TypeAnnotated::new(name, typ).to_document(parent, layout);

                if layout.unknown_ids {
                    Document::concat([
                        Document::text("["),
                        Document::text(format!("{:?}", self.0.as_ptr())),
                        Document::text("]"),
                        doc,
                    ])
                } else {
                    doc
                }
            }
            ExprVariants::Link { target } => target.to_document(parent, layout),
        }
    }
}
