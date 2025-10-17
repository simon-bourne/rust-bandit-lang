use super::{Term, TermEnum, Variable};
use crate::{
    Pretty,
    pretty::{Document, Layout, Operator, Side, has_type},
};

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
        has_type(self.name, &self.typ).to_document(parent, layout)
    }
}
