use derive_more::Constructor;

use super::Term;
use crate::{
    Pretty,
    pretty::{BinaryOperator, Document, Layout, Operator, Side, TypeAnnotated, pretty_let},
    typed::TermEnum,
};

impl<'src> Pretty for Term<'src> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        match &*self.clone().value() {
            TermEnum::Type => Document::text("Type"),
            TermEnum::Apply {
                function,
                argument,
                typ,
                evaluation,
            } => has_type(
                BinaryOperator(function, Operator::Apply(*evaluation), argument),
                typ,
            )
            .to_document(parent, layout),
            TermEnum::Variable { name, typ, .. } => {
                has_type(WithId::new(self, name), typ).to_document(parent, layout)
            }
            TermEnum::Constant { name, typ } => {
                has_type(WithId::new(self, name), typ).to_document(parent, layout)
            }
            TermEnum::Unknown { typ, .. } => {
                has_type(WithId::new(self, None), typ).to_document(parent, layout)
            }
            TermEnum::Let { value, binding } => pretty_let(value, binding, parent, layout),
            TermEnum::Pi(binding) => {
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
            TermEnum::Lambda(binding) => binding.to_document(r"\", parent, layout),
        }
    }
}

#[derive(Constructor)]
struct WithId<'a, 'src, Value> {
    id: &'a Term<'src>,
    value: Value,
}

impl<'a, 'src, Value: Pretty> Pretty for WithId<'a, 'src, Value> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        let name_doc = self.value.to_document(parent, layout);

        if layout.show_id {
            Document::concat([
                name_doc,
                Document::text("["),
                Document::text(format!("{:?}", self.id.0.id())),
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
