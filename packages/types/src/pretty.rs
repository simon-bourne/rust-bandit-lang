use std::fmt;

use derive_more::Constructor;
use pretty::RcDoc;

use super::{GenericTerm, TermReference, VariableBinding};
use crate::Evaluation;

pub trait Pretty {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document;

    fn to_pretty_string(&self, width: usize) -> String {
        self.to_document(None, Layout::default())
            .pretty(width)
            .to_string()
    }

    fn to_verbose_string(&self, width: usize) -> String {
        self.to_document(None, Layout::verbose())
            .pretty(width)
            .to_string()
    }

    fn debug(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let width = 80;
        let terse = self.to_pretty_string(width);
        let verbose = self.to_verbose_string(width);
        write!(f, "{terse} {{-- {verbose} --}}")
    }
}

impl<T: Pretty> Pretty for &'_ T {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        (*self).to_document(parent, layout)
    }
}

impl<'src, Term: TermReference<'src>> VariableBinding<'src, Term> {
    fn to_document(
        &self,
        binder: &str,
        parent: Option<(Operator, Side)>,
        layout: Layout,
    ) -> Document {
        parenthesize_if(
            parent.is_some(),
            [
                Document::as_string(binder),
                has_type(self.name, &self.binding_type()).to_document(None, layout),
                Document::text(" ⇒ "),
                self.in_term.to_document(None, layout),
            ],
        )
    }
}

#[derive(Constructor)]
pub struct TypeAnnotated<Value: Pretty, Type: Pretty> {
    value: Value,
    typ: Option<Type>,
}

pub fn has_type<'a, 'src, Value, Type>(
    value: Value,
    typ: &'a Type,
) -> TypeAnnotated<Value, &'a Type>
where
    Value: Pretty,
    Type: TermReference<'src> + 'a,
{
    TypeAnnotated {
        value,
        typ: typ.is_known().then_some(typ),
    }
}

impl<Value: Pretty, Type: Pretty> Pretty for TypeAnnotated<Value, Type> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        if layout.annotate_type
            && let Some(typ) = self.typ.as_ref()
        {
            return Operator::HasType.to_document(
                parent,
                &self.value,
                &typ,
                layout,
                layout.without_types(),
            );
        }
        self.value.to_document(parent, layout)
    }
}

impl<'src, Term: TermReference<'src>> Pretty for GenericTerm<'src, Term> {
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
            Self::Variable(variable) => WithId::new(self, variable).to_document(parent, layout),
            Self::Unknown { typ } => {
                has_type(WithId::new(self, None), typ).to_document(parent, layout)
            }
            Self::Let { value, binding } => {
                let typ = &value.typ();

                let assignment = Operator::Equals.to_document(
                    None,
                    &has_type(binding.name, typ),
                    value,
                    layout,
                    layout.without_types().variable_value(),
                );

                let symbol = match binding.evaluation {
                    Evaluation::Static => "static",
                    Evaluation::Dynamic => "let",
                };

                parenthesize_if(
                    parent.is_some(),
                    [
                        Document::text(symbol),
                        Document::text(" "),
                        assignment,
                        Document::text(" ⇒ "),
                        binding.in_term.to_document(None, layout),
                    ],
                )
            }
            Self::Pi(binding) => {
                if binding.name.is_none() {
                    let layout = layout.without_types();
                    Operator::Arrow(binding.evaluation).to_document(
                        parent,
                        &binding.binding_type(),
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

impl Pretty for str {
    fn to_document(&self, _parent: Option<(Operator, Side)>, _layout: Layout) -> Document {
        Document::as_string(self)
    }
}

impl Pretty for &str {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        (*self).to_document(parent, layout)
    }
}

impl Pretty for Option<&str> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        self.unwrap_or("_").to_document(parent, layout)
    }
}

fn parenthesize_if(condition: bool, docs: impl IntoIterator<Item = Document>) -> Document {
    let docs = Document::concat(docs);

    if condition {
        Document::concat([Document::text("("), docs, Document::text(")")])
    } else {
        docs
    }
}

pub struct BinaryOperator<Left: Pretty, Right: Pretty>(pub Left, pub Operator, pub Right);

impl<Left: Pretty, Right: Pretty> Pretty for BinaryOperator<Left, Right> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        self.1.to_document(parent, &self.0, &self.2, layout, layout)
    }
}

#[derive(Copy, Clone)]
pub enum Operator {
    Equals,
    HasType,
    Arrow(Evaluation),
    Apply(Evaluation),
}

impl Operator {
    pub fn symbol(self) -> &'static str {
        match self {
            Self::Equals => " = ",
            Self::HasType => " : ",
            Self::Arrow(Evaluation::Dynamic) => " → ",
            Self::Arrow(Evaluation::Static) => " ⇒ ",
            Self::Apply(Evaluation::Dynamic) => " ",
            Self::Apply(Evaluation::Static) => " @ ",
        }
    }

    pub fn to_document(
        self,
        parent: Option<(Operator, Side)>,
        left: &impl Pretty,
        right: &impl Pretty,
        left_layout: Layout,
        right_layout: Layout,
    ) -> Document {
        parenthesize_if(
            self.parenthesize(parent),
            [
                left.to_document(Some((self, Side::Left)), left_layout),
                Document::text(self.symbol()),
                right.to_document(Some((self, Side::Right)), right_layout),
            ],
        )
    }

    fn parenthesize(self, parent: Option<(Operator, Side)>) -> bool {
        let Some((parent, side)) = parent else {
            return false;
        };

        if self.precedence() > parent.precedence() {
            return false;
        }

        if self.precedence() < parent.precedence() {
            return true;
        }

        assert!(self.precedence() == parent.precedence());

        // We shouldn't have operators of the same precedence but different
        // associativity. Just in case:
        if self.associativity() != parent.associativity() {
            return true;
        }

        if self.associativity() == side {
            return false;
        }

        true
    }

    fn precedence(self) -> usize {
        match self {
            Self::Equals => 0,
            Self::HasType => 1,
            Self::Arrow(_) => 2,
            Self::Apply(Evaluation::Dynamic) => 3,
            Self::Apply(Evaluation::Static) => 4,
        }
    }

    fn associativity(self) -> Side {
        match self {
            Self::Equals => Side::Right,
            Self::HasType => Side::Right,
            Self::Arrow(_) => Side::Right,
            Self::Apply(_) => Side::Left,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Side {
    Left,
    Right,
}

pub type Document = RcDoc<'static>;

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Layout {
    pub annotate_type: bool,
    pub show_id: bool,
    pub show_unknown: bool,
    pub variable: LayoutVariable,
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum LayoutVariable {
    Name,
    Value,
}

impl Layout {
    pub fn verbose() -> Self {
        Self {
            annotate_type: true,
            show_id: true,
            show_unknown: true,
            variable: LayoutVariable::Name,
        }
    }

    pub fn without_types(self) -> Self {
        Self {
            annotate_type: false,
            ..self
        }
    }

    pub fn variable_value(self) -> Self {
        Self {
            variable: LayoutVariable::Value,
            ..self
        }
    }
}

impl Default for Layout {
    fn default() -> Self {
        Self {
            annotate_type: true,
            show_id: false,
            show_unknown: false,
            variable: LayoutVariable::Name,
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
