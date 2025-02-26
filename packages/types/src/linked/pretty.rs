use super::{Term, TermEnum, Variable, VariableId, VariableValue};
use crate::{
    Pretty,
    pretty::{Document, Layout, LayoutVariable, Operator, Side, has_type},
};

struct WithId<'src, Value> {
    id: Option<VariableId<'src>>,
    value: Value,
}

impl<Value: Pretty> Pretty for WithId<'_, Value> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        let name_doc = self.value.to_document(parent, layout);

        match &self.id {
            Some(id) if layout.show_id => Document::concat([
                name_doc,
                Document::text("["),
                Document::text(format!("{:?}", id.key())),
                Document::text("]"),
            ]),
            _ => name_doc,
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
        let id = self.id.clone();

        if let (LayoutVariable::Value, VariableValue::Known { value }) =
            (layout.variable, &self.value)
        {
            has_type(WithId { id, value }, &self.typ()).to_document(parent, layout)
        } else {
            has_type(
                WithId {
                    value: self.id.as_ref().map(|id| id.as_ref()),
                    id,
                },
                &self.typ(),
            )
            .to_document(parent, layout)
        }
    }
}
