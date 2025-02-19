use std::ptr;

use super::{Term, TermEnum, Variable, VariableValue};
use crate::{
    pretty::{Document, Layout, LayoutVariable, Operator, Side, TypeAnnotated},
    Pretty,
};

struct WithId<Id, Value> {
    id: *const Id,
    value: Value,
}

impl<Id, Value: Pretty> Pretty for WithId<Id, Value> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        let name_doc = self.value.to_document(parent, layout);

        if layout.show_id {
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
            TermEnum::Value { term, .. } => term.to_document(parent, layout),
            TermEnum::Link { target } => target.to_document(parent, layout),
        }
    }
}

impl Pretty for Variable<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        let id = ptr::from_ref(self);

        if let (LayoutVariable::Value, VariableValue::Known { value }) =
            (layout.variable, &self.value)
        {
            TypeAnnotated::new(WithId { id, value }, &self.typ()).to_document(parent, layout)
        } else {
            TypeAnnotated::new(
                WithId {
                    value: self.name,
                    id,
                },
                &self.typ(),
            )
            .to_document(parent, layout)
        }
    }
}
