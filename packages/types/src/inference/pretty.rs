use std::ptr;

use super::{Term, TermVariants, Variable};
use crate::{
    pretty::{Document, Layout, Operator, Side, TypeAnnotated},
    Pretty,
};

struct WithId<Id, Value> {
    id: *const Id,
    value: Value,
}

impl<Id, Value: Pretty> Pretty for WithId<Id, Value> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        let name_doc = self.value.to_document(parent, layout);

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
                TypeAnnotated::new(WithId { value: name, id }, typ).to_document(parent, layout)
            }
            TermVariants::Link { target } => target.to_document(parent, layout),
        }
    }
}

impl Pretty for Variable<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        match self {
            Self::Free { typ } => TypeAnnotated::new(
                WithId {
                    value: None,
                    id: ptr::from_ref(self),
                },
                typ,
            )
            .to_document(parent, layout),
            Self::Bound { name, value, typ } => {
                TypeAnnotated::new(WithId { value, id: name }, typ).to_document(parent, layout)
            }
        }
    }
}
