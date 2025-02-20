use super::Term;
use crate::{
    Pretty,
    pretty::{Document, Layout, Operator, Side},
};

impl Pretty for Term<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        self.0.borrow().to_document(parent, layout)
    }
}
