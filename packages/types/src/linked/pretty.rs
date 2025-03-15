use super::{Term, TermEnum, VariableName};
use crate::{
    Pretty,
    pretty::{Document, Layout, Operator, Side},
};

impl Pretty for Term<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        match &*self.0.borrow() {
            TermEnum::Value { term, .. } => term.to_document(parent, layout),
            TermEnum::Link { target } => target.to_document(parent, layout),
        }
    }
}

impl Pretty for VariableName<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        self.name.to_document(parent, layout)
    }
}
