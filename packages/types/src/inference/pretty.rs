use super::{ExprVariants, Expression};
use crate::{
    pretty::{Document, Layout, Operator, Side, TypeAnnotated},
    Pretty,
};

struct WithId<'src> {
    name: Option<&'src str>,
    id: *mut ExprVariants<'src>,
}

impl Pretty for WithId<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        let name_doc = self.name.to_document(parent, layout);

        if layout.unknown_ids {
            Document::concat([
                name_doc,
                Document::text("["),
                Document::text(format!("{:?}", self.id)),
                Document::text("]"),
            ])
        } else {
            name_doc
        }
    }
}

impl Pretty for Expression<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        match &*self.0.borrow() {
            ExprVariants::Known { expression, .. } => expression.to_document(parent, layout),
            ExprVariants::Unknown { name, typ } => {
                let name = *name;
                let id = self.0.as_ptr();
                TypeAnnotated::new(WithId { name, id }, typ).to_document(parent, layout)
            }
            ExprVariants::Link { target } => target.to_document(parent, layout),
        }
    }
}
