use std::ptr;

use super::{Term, TermVariants, Variable};
use crate::{
    pretty::{Document, Layout, Operator, Side, TypeAnnotated},
    Pretty, TermReference,
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
            TermVariants::Value { term, .. } => term.to_document(parent, layout),
            TermVariants::Link { target } => target.to_document(parent, layout),
        }
    }
}

impl Pretty for Variable<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        let id = ptr::from_ref(self);
        match self {
            // TODO: Need to think about whether we want to keep variables around.
            Self::Bound { name, value } => {
                if value.is_known() {
                    value.to_document(parent, layout)
                } else {
                    WithId { value: name, id }.to_document(parent, layout)
                }
            }
            Self::Free { typ } => {
                TypeAnnotated::new(WithId { value: None, id }, typ).to_document(parent, layout)
            }
        }
    }
}
