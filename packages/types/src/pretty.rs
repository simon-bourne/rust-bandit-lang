use pretty::RcDoc;

use super::{ExpressionReference, GenericExpression, VariableBinding};
use crate::{Variable, VariableReference};

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

    fn debug(&self) -> String {
        let width = 80;
        let terse = self.to_pretty_string(width);
        let verbose = self.to_verbose_string(width);
        format!("{terse} {{-- {verbose} --}}")
    }
}

impl<T: Pretty> Pretty for &'_ T {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        (*self).to_document(parent, layout)
    }
}

impl Pretty for Variable<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        self.name.to_document(parent, layout)
    }
}

impl<'src, Expr: ExpressionReference<'src>> VariableBinding<'src, Expr> {
    fn to_document(&self, kind: Document, layout: Layout) -> Document {
        Document::concat([
            kind,
            variable_to_document(self.name, &self.variable_value, None, layout),
            Document::text(" ⇒ "),
            self.in_expression.to_document(None, layout),
        ])
    }
}

pub struct TypeAnnotated<Value: Pretty, Type: Pretty> {
    value: Value,
    typ: Option<Type>,
}

impl<'a, 'src, Value, Type> TypeAnnotated<Value, &'a Type>
where
    Value: Pretty,
    Type: ExpressionReference<'src> + 'a,
{
    pub fn new(value: Value, typ: &'a Type) -> Self {
        Self {
            value,
            typ: typ.is_known().then_some(typ),
        }
    }
}

impl<Value: Pretty, Type: Pretty> Pretty for TypeAnnotated<Value, Type> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        if layout.annotate_types {
            if let Some(typ) = self.typ.as_ref() {
                return Operator::HasType.to_document(
                    parent,
                    &self.value,
                    &typ,
                    layout,
                    layout.without_types(),
                );
            }
        }
        self.value.to_document(parent, layout)
    }
}

pub fn variable_to_document<'src>(
    name: Option<&str>,
    value: &impl ExpressionReference<'src>,
    parent: Option<(Operator, Side)>,
    layout: Layout,
) -> Document {
    let typ = &value.typ();
    let type_annotated = TypeAnnotated::new(name, typ);

    if value.is_known() || layout.show_unknowns {
        Operator::Equals.to_document(
            parent,
            &type_annotated,
            value,
            layout,
            layout.without_types(),
        )
    } else {
        type_annotated.to_document(parent, layout)
    }
}

impl<'src, Expr: ExpressionReference<'src>> Pretty for GenericExpression<'src, Expr> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        match self {
            Self::TypeOfType => Document::text("Type"),
            Self::Constant { name, typ } => {
                TypeAnnotated::new(*name, typ).to_document(parent, layout)
            }
            Self::Apply {
                function,
                argument,
                typ,
            } => TypeAnnotated::new(BinaryOperator(function, Operator::Apply, argument), typ)
                .to_document(parent, layout),
            Self::Let(binding) => parenthesize_if(
                parent.is_some(),
                binding.to_document(
                    Document::concat([Document::text("let"), Document::space()]),
                    layout,
                ),
            ),
            Self::Pi(binding) => {
                let variable_name = binding.name;

                if variable_name.is_some() {
                    parenthesize_if(
                        parent.is_some(),
                        binding.to_document(Document::text("∀"), layout),
                    )
                } else {
                    let layout = Layout::default().without_types();
                    Operator::Arrow.to_document(
                        parent,
                        &binding.variable_value.typ(),
                        &binding.in_expression,
                        layout,
                        layout,
                    )
                }
            }
            Self::Lambda(binding) => parenthesize_if(
                parent.is_some(),
                binding.to_document(Document::text("\\"), layout),
            ),
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

impl<'src, Value: ExpressionReference<'src>> Pretty for VariableReference<'src, Value> {
    fn to_document(&self, parent: Option<(Operator, Side)>, layout: Layout) -> Document {
        variable_to_document(Some(self.name), &self.value, parent, layout)
    }
}

fn parenthesize_if(condition: bool, docs: Document) -> Document {
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
    Arrow,
    Apply,
}

impl Operator {
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
            Document::concat([
                left.to_document(Some((self, Side::Left)), left_layout),
                Document::text(match self {
                    Self::Equals => " = ",
                    Self::HasType => " : ",
                    Self::Arrow => " → ",
                    Self::Apply => " ",
                }),
                right.to_document(Some((self, Side::Right)), right_layout),
            ]),
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
            Self::Arrow => 2,
            Self::Apply => 3,
        }
    }

    fn associativity(self) -> Side {
        match self {
            Self::Equals => Side::Right,
            Self::HasType => Side::Right,
            Self::Arrow => Side::Right,
            Self::Apply => Side::Left,
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
    pub annotate_types: bool,
    pub unknown_ids: bool,
    pub show_unknowns: bool,
}

impl Layout {
    fn verbose() -> Self {
        Self {
            annotate_types: true,
            unknown_ids: true,
            show_unknowns: true,
        }
    }

    fn without_types(self) -> Self {
        Self {
            annotate_types: false,
            ..self
        }
    }
}

impl Default for Layout {
    fn default() -> Self {
        Self {
            annotate_types: true,
            unknown_ids: false,
            show_unknowns: false,
        }
    }
}
