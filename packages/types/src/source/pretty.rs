use crate::{
    Pretty,
    pretty::{Document, Layout, Operator, Side, TypeAnnotated, has_type},
    source::{FunctionDefinition, Term, TermEnum},
};

impl Pretty for Term<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        match self.0.as_ref() {
            TermEnum::Value { term } => term.to_document(parent, layout),
            TermEnum::HasType { term, typ } => has_type(term, typ).to_document(parent, layout),
        }
    }
}

impl Pretty for FunctionDefinition<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        let name = TypeAnnotated::new(self.name, self.typ.clone());

        if let Some(value) = self.value.as_ref() {
            Operator::Equals.to_document(parent, &name, &value, layout, layout.without_types())
        } else {
            name.to_document(parent, layout)
        }
    }
}
