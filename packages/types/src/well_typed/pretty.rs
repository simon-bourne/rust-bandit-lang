use super::Expression;
use crate::{
    pretty::{Document, Layout, Operator, Side},
    Pretty,
};

impl Pretty for Expression<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        self.0.borrow().to_document(parent, layout)
    }
}
