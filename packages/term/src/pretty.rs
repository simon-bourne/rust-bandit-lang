use std::fmt;

use derive_more::Constructor;
use pretty::RcDoc;

use super::VariableBinding;
use crate::{ArgumentStyle, Variable};

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

impl<Term> VariableBinding<Term, ArgumentStyle>
where
    Term: Variable + Pretty,
    Term::Declaration: Pretty,
{
    pub fn pi_to_document(
        &self,
        parent: Option<(Operator, Side)>,
        layout: Layout,
    ) -> RcDoc<'static> {
        let binder = match self.discriminator {
            ArgumentStyle::Implicit => " ⇒ ",
            ArgumentStyle::Explicit => " → ",
        };

        self.to_document("∀", binder, parent, layout)
    }

    pub fn lambda_to_document(
        &self,
        parent: Option<(Operator, Side)>,
        layout: Layout,
    ) -> RcDoc<'static> {
        let prefix = match self.discriminator {
            ArgumentStyle::Implicit => r"\\",
            ArgumentStyle::Explicit => r"\",
        };

        self.to_document(prefix, " = ", parent, layout)
    }

    fn to_document(
        &self,
        prefix: &'static str,
        binder: &'static str,
        parent: Option<(Operator, Side)>,
        layout: Layout,
    ) -> RcDoc<'static> {
        parenthesize_unary_prefix(
            [
                Document::text(prefix),
                self.variable.to_document(None, layout),
                Document::text(binder),
                self.in_term.to_document(None, layout),
            ],
            parent,
        )
    }
}

impl<Term> VariableBinding<Term, ()>
where
    Term: Variable + Pretty,
    Term::Declaration: Pretty,
{
    pub fn let_to_document(
        &self,
        value: &impl Pretty,
        parent: Option<(Operator, Side)>,
        layout: Layout,
    ) -> Document {
        parenthesize_unary_prefix(
            [
                Document::text("let"),
                Document::text(" "),
                self.variable.to_document(None, layout),
                Document::text(" = "),
                value.to_document(None, layout.without_types()),
                Document::text(" in "),
                self.in_term.to_document(None, layout),
            ],
            parent,
        )
    }
}

// A unary prefix operator with the lowest priority
fn parenthesize_unary_prefix(
    docs: impl IntoIterator<Item = Document>,
    parent: Option<(Operator, Side)>,
) -> RcDoc<'static> {
    parenthesize_if(matches!(parent, Some((_, Side::Left))), docs)
}

#[derive(Constructor)]
pub struct TypeAnnotated<Value: Pretty, Type: Pretty> {
    value: Value,
    typ: Option<Type>,
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

pub fn parenthesize_if(condition: bool, docs: impl IntoIterator<Item = Document>) -> Document {
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
    HasType,
    Arrow(ArgumentStyle),
    Apply(ArgumentStyle),
}

impl Operator {
    pub fn symbol(self) -> &'static str {
        match self {
            Self::HasType => " : ",
            Self::Arrow(ArgumentStyle::Explicit) => " → ",
            Self::Arrow(ArgumentStyle::Implicit) => " ⇒ ",
            Self::Apply(ArgumentStyle::Explicit) => " ",
            Self::Apply(ArgumentStyle::Implicit) => " @ ",
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
            Self::HasType => 0,
            Self::Arrow(_) => 1,
            Self::Apply(ArgumentStyle::Explicit) => 2,
            Self::Apply(ArgumentStyle::Implicit) => 3,
        }
    }

    fn associativity(self) -> Side {
        match self {
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
}

impl Layout {
    pub fn verbose() -> Self {
        Self {
            annotate_type: true,
            show_id: true,
            show_unknown: true,
        }
    }

    pub fn without_types(self) -> Self {
        Self {
            annotate_type: false,
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
        }
    }
}
