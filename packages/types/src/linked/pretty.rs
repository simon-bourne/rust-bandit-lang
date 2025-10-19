use super::{IndirectTerm, Term};
use crate::{
    Pretty,
    linked::TermEnum,
    pretty::{BinaryOperator, Document, Layout, Operator, Side, TypeAnnotated, pretty_let},
};

impl Pretty for Term<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        match &*self.0.borrow() {
            IndirectTerm::Value { term, .. } => term.to_document(parent, layout),
            IndirectTerm::Link { target } => target.to_document(parent, layout),
        }
    }
}

impl<'src> Pretty for TermEnum<'src> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        match self {
            Self::Type => Document::text("Type"),
            Self::Apply {
                function,
                argument,
                typ,
                evaluation,
            } => has_type(
                BinaryOperator(function, Operator::Apply(*evaluation), argument),
                typ,
            )
            .to_document(parent, layout),
            Self::Variable { name, typ, .. } => {
                WithId::new(self, has_type(name, typ)).to_document(parent, layout)
            }
            Self::Constant { name, value } => {
                WithId::new(self, has_type(name, &value.typ())).to_document(parent, layout)
            }
            Self::Unknown { typ } => {
                has_type(WithId::new(self, None), typ).to_document(parent, layout)
            }
            Self::Let { value, binding } => pretty_let(value, binding, parent, layout),
            Self::Pi(binding) => {
                if binding.variable_name().is_none() {
                    let layout = layout.without_types();
                    Operator::Arrow(binding.evaluation).to_document(
                        parent,
                        &binding.variable.typ(),
                        &binding.in_term,
                        layout,
                        layout,
                    )
                } else {
                    binding.to_document("∀", parent, layout)
                }
            }
            // TODO: Static (`=>`) and dynamic (`->`) bindings
            Self::Lambda(binding) => binding.to_document(r"\", parent, layout),
        }
    }
}

struct WithId<Id, Value> {
    id: *const Id,
    value: Value,
}

impl<Id, Value> WithId<Id, Value> {
    fn new(id: *const Id, value: Value) -> Self {
        Self { id, value }
    }
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

fn has_type<'a, 'src, Value: Pretty>(
    value: Value,
    typ: &'a Term<'src>,
) -> TypeAnnotated<Value, &'a Term<'src>> {
    TypeAnnotated::new(value, typ.is_known().then_some(typ))
}
