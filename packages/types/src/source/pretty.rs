use crate::{
    Evaluation, Pretty,
    pretty::{Document, Layout, Operator, Side, TypeAnnotated, pretty_let},
    source::{FunctionDefinition, Term, TermEnum},
};

impl Pretty for Term<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        match self.0.as_ref() {
            TermEnum::Type => Document::text("Type"),
            TermEnum::Apply {
                function,
                argument,
                evaluation,
            } => {
                Operator::Apply(*evaluation).to_document(parent, function, argument, layout, layout)
            }
            TermEnum::Variable(name) => Document::as_string(name),
            TermEnum::Unknown => Document::text("_"),
            TermEnum::Let { value, binding } => pretty_let(value, binding, parent, layout),
            TermEnum::Pi(binding) => binding.to_document("∀", parent, layout),
            TermEnum::FunctionType(input_type, output_type) => Operator::Arrow(Evaluation::Dynamic)
                .to_document(parent, input_type, output_type, layout, layout),
            TermEnum::Lambda(binding) => binding.to_document("\\", parent, layout),
            TermEnum::HasType { term, typ } => {
                Operator::HasType.to_document(parent, term, typ, layout, layout)
            }
        }
    }
}

impl Pretty for FunctionDefinition<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        let name = TypeAnnotated::new(self.name, self.typ.as_ref());

        Operator::Equals.to_document(parent, &name, &self.value, layout, layout.without_types())
    }
}
