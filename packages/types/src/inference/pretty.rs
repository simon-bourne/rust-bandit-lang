use super::{TermVariants, Term};
use crate::{
    pretty::{Document, Layout, Operator, Side, TypeAnnotated},
    Pretty,
};

struct WithId<'src> {
    name: Option<&'src str>,
    id: *mut TermVariants<'src>,
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

impl Pretty for Term<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        match &*self.0.borrow() {
            TermVariants::Known { term, .. } => term.to_document(parent, layout),
            TermVariants::Unknown { name, typ } => {
                let name = *name;
                let id = self.0.as_ptr();
                TypeAnnotated::new(WithId { name, id }, typ).to_document(parent, layout)
            }
            TermVariants::Link { target } => target.to_document(parent, layout),
        }
    }
}
