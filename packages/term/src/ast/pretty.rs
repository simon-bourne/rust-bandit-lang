use crate::{
    ArgumentStyle, Pretty,
    ast::{Data, Definition, Function, Term, TermEnum, VariableDeclaration},
    pretty::{Document, Layout, Operator, Side, TypeAnnotated},
};

impl Pretty for Term<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        match self.0.as_ref() {
            TermEnum::Type => Document::text("Type"),
            TermEnum::Apply { function, argument } => Operator::Apply(ArgumentStyle::Explicit)
                .to_document(parent, function, argument, layout, layout),
            TermEnum::Variable(name) => Document::as_string(name),
            TermEnum::Unknown => Document::text("_"),
            TermEnum::Let { value, binding } => binding.let_to_document(value, parent, layout),
            TermEnum::Pi(binding) => binding.pi_to_document(parent, layout),
            TermEnum::FunctionType(input_type, output_type, arg_style) => Operator::Arrow(
                *arg_style,
            )
            .to_document(parent, input_type, output_type, layout, layout),
            TermEnum::Lambda(binding) => binding.lambda_to_document(parent, layout),
            TermEnum::HasType { term, typ } => {
                Operator::HasType.to_document(parent, term, typ, layout, layout)
            }
            TermEnum::SpecifyImplicits(term) => Document::concat([
                Document::text("["),
                term.to_document(None, layout),
                Document::text("]"),
            ]),
        }
    }
}

impl Pretty for VariableDeclaration<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        TypeAnnotated::new(self.name, self.typ.as_ref()).to_document(parent, layout)
    }
}

impl Pretty for Definition<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        match self {
            Definition::Function(f) => f.to_document(parent, layout),
            Definition::Data(data) => data.to_document(parent, layout),
        }
    }
}

impl Pretty for Function<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        let name = TypeAnnotated::new(self.name, self.typ.as_ref());

        Operator::Equals.to_document(parent, &name, &self.value, layout, layout.without_types())
    }
}

impl Pretty for Data<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        let name = TypeAnnotated::new(
            self.type_constructor.name,
            self.type_constructor.typ.as_ref(),
        );

        Document::concat([
            Document::text("data"),
            Document::text(" "),
            name.to_document(parent, layout),
        ])
    }
}
