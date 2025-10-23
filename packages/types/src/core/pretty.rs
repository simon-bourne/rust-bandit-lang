use super::Term;
use crate::{
    Pretty,
    pretty::{Document, Layout, Operator, Side},
};

impl<'src> Pretty for Term<'src> {
    fn to_document(&self, _parent: Option<(Operator, Side)>, _layout: Layout) -> Document {
        todo!()
    }
}
