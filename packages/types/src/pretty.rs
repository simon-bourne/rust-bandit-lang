use std::fmt;

use pretty::RcDoc;

use crate::{
    context::{Variable, VariableReference},
    ExprRefVariants, Expression, ExpressionRef, Stage, VariableBinding,
};

pub trait Pretty {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotations: Annotation) -> Document;

    fn type_to_document(
        &self,
        parent: Option<(Operator, Side)>,
        annotations: Annotation,
    ) -> Document;

    fn annotate_with_type(
        &self,
        typ: &(impl Pretty + ?Sized),
        parent: Option<(Operator, Side)>,
        annotation: Annotation,
    ) -> Document {
        annotate_with_type(
            |parent| self.to_document(parent, Annotation::Off),
            typ,
            parent,
            annotation,
        )
    }

    fn is_known(&self) -> bool;

    fn to_pretty_string(&self, width: usize) -> String {
        self.to_document(None, Annotation::On)
            .pretty(width)
            .to_string()
    }
}

fn annotate_with_type(
    to_doc: impl FnOnce(Option<(Operator, Side)>) -> Document,
    typ: &(impl ?Sized + Pretty),
    parent: Option<(Operator, Side)>,
    annotation: Annotation,
) -> Document {
    if matches!(annotation, Annotation::Off) {
        return to_doc(parent);
    }

    let op = Operator::HasType;

    disambiguate(
        Some(op),
        parent,
        [
            to_doc(Some((op, Side::Left))),
            Document::text(" : "),
            typ.to_document(Some((op, Side::Right)), Annotation::Off),
        ],
    )
}

impl fmt::Debug for ExpressionRef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.to_pretty_string(80))
    }
}

impl Pretty for ExpressionRef<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotation: Annotation) -> Document {
        match &*self.0.borrow() {
            ExprRefVariants::Known { expression } => expression.to_document(parent, annotation),
            ExprRefVariants::Unknown { typ } => "_".annotate_with_type(typ, parent, annotation),
            ExprRefVariants::Link { target } => target.to_document(parent, annotation),
        }
    }

    fn type_to_document(
        &self,
        parent: Option<(Operator, Side)>,
        annotations: Annotation,
    ) -> Document {
        self.typ().to_document(parent, annotations)
    }

    fn is_known(&self) -> bool {
        match &*self.0.borrow() {
            ExprRefVariants::Known { .. } => true,
            ExprRefVariants::Unknown { .. } => false,
            ExprRefVariants::Link { target } => target.is_known(),
        }
    }
}

impl<'src, S: Stage<'src>> Pretty for Expression<'src, S> {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotation: Annotation) -> Document {
        let binding_doc = |binding: &VariableBinding<'src, S>| {
            Document::concat([
                Document::text("{"),
                Document::as_string(binding.name),
                Document::text(" = "),
                binding.variable_value.to_document(None, Annotation::On),
                Document::text("}"),
            ])
        };
        match self {
            Self::Literal(literal) => Document::as_string(literal),
            Self::Apply {
                function,
                argument,
                typ,
            } => {
                let op = Operator::Apply;

                annotate_with_type(
                    |parent| {
                        disambiguate(
                            Some(Operator::Apply),
                            parent,
                            [
                                function.to_document(Some((op, Side::Left)), annotation),
                                Document::space(),
                                argument.to_document(Some((op, Side::Right)), annotation),
                            ],
                        )
                    },
                    typ,
                    parent,
                    annotation,
                )
            }
            // TODO(to_doc): Fix bindings"
            Self::Let(binding) => parenthesize_if(
                parent.is_some(),
                [
                    Document::text("let"),
                    Document::space(),
                    binding_doc(binding),
                    Document::text(" in "),
                    binding.in_expression.to_document(None, annotation),
                ],
            ),
            // TODO(to_doc): Fix bindings"
            Self::FunctionType(binding) => {
                let variable_name = &binding.name;

                if variable_name.is_known() {
                    parenthesize_if(
                        parent.is_some(),
                        [
                            Document::text("∀"),
                            binding_doc(binding),
                            Document::text(" ⇒ "),
                            binding.in_expression.to_document(None, annotation),
                        ],
                    )
                } else {
                    // TODO: Operator::Arrow.to_document(binding.variable_value,
                    // binding.in_expression)
                    let op = Operator::Arrow;
                    disambiguate(
                        Some(op),
                        parent,
                        [
                            binding
                                .variable_value
                                .type_to_document(Some((op, Side::Left)), Annotation::Off),
                            Document::text(" → "),
                            binding
                                .in_expression
                                .to_document(Some((op, Side::Right)), Annotation::Off),
                        ],
                    )
                }
            }
            // TODO(to_doc): Fix bindings"
            Self::Lambda(binding) => parenthesize_if(
                parent.is_some(),
                [
                    Document::text("\\"),
                    binding_doc(binding),
                    Document::text(" ⇒ "),
                    binding.in_expression.to_document(None, annotation),
                ],
            ),
            Self::Variable(var) => var.to_document(parent, annotation),
        }
    }

    fn type_to_document(
        &self,
        parent: Option<(Operator, Side)>,
        // TODO: Remove this param?
        annotations: Annotation,
    ) -> Document {
        match self {
            Self::Literal(literal) => Document::as_string(literal),
            Self::Apply { typ, .. } => typ.to_document(parent, annotations),
            Self::Let(variable_binding)
            | Self::FunctionType(variable_binding)
            | Self::Lambda(variable_binding) => variable_binding
                .variable_value
                .type_to_document(parent, annotations),
            Self::Variable(variable) => variable.type_to_document(parent, annotations),
        }
    }

    fn is_known(&self) -> bool {
        true
    }
}

impl fmt::Debug for VariableReference<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.to_pretty_string(80))
    }
}

impl Pretty for VariableReference<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotation: Annotation) -> Document {
        // TODO(to_doc): "x[ : T][ = e]"
        self.name()
            .annotate_with_type(&self.value.typ(), parent, annotation)
    }

    fn type_to_document(
        &self,
        parent: Option<(Operator, Side)>,
        annotations: Annotation,
    ) -> Document {
        self.value.type_to_document(parent, annotations)
    }

    fn is_known(&self) -> bool {
        self.name().is_known()
    }
}

impl fmt::Debug for Variable<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.to_pretty_string(80))
    }
}

impl Pretty for Variable<'_> {
    fn to_document(
        &self,
        parent: Option<(crate::pretty::Operator, crate::pretty::Side)>,
        annotation: crate::pretty::Annotation,
    ) -> crate::pretty::Document {
        self.name().to_document(parent, annotation)
    }

    fn type_to_document(
        &self,
        parent: Option<(Operator, Side)>,
        annotations: Annotation,
    ) -> Document {
        self.name().type_to_document(parent, annotations)
    }

    fn is_known(&self) -> bool {
        self.name().is_known()
    }
}

impl Pretty for str {
    fn to_document(
        &self,
        _parent: Option<(crate::pretty::Operator, crate::pretty::Side)>,
        _annotation: crate::pretty::Annotation,
    ) -> crate::pretty::Document {
        Document::as_string(self)
    }

    fn type_to_document(
        &self,
        parent: Option<(Operator, Side)>,
        annotations: Annotation,
    ) -> Document {
        "_".to_document(parent, annotations)
    }

    fn is_known(&self) -> bool {
        self != "_"
    }
}

impl Pretty for &str {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotation: Annotation) -> Document {
        (*self).to_document(parent, annotation)
    }

    fn type_to_document(
        &self,
        parent: Option<(Operator, Side)>,
        annotations: Annotation,
    ) -> Document {
        (*self).type_to_document(parent, annotations)
    }

    fn is_known(&self) -> bool {
        (*self).is_known()
    }
}

pub fn disambiguate(
    operator: Option<Operator>,
    parent: Option<(Operator, Side)>,
    docs: impl IntoIterator<Item = Document>,
) -> Document {
    parenthesize_if(operator.is_some_and(|op| op.parenthesize(parent)), docs)
}

fn parenthesize_if(condition: bool, docs: impl IntoIterator<Item = Document>) -> Document {
    if condition {
        parenthesize(docs)
    } else {
        Document::concat(docs)
    }
}

fn parenthesize(docs: impl IntoIterator<Item = Document>) -> Document {
    Document::concat([
        Document::text("("),
        Document::concat(docs),
        Document::text(")"),
    ])
}

#[derive(Copy, Clone)]
pub enum Operator {
    Equals,
    HasType,
    Arrow,
    Apply,
}

impl Operator {
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

#[derive(Copy, Clone)]
pub enum Annotation {
    On,
    Off,
}
