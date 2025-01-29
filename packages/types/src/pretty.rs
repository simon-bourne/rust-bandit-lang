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

    Operator::HasType.to_document(parent, to_doc, typ, Annotation::Off)
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
            } => annotate_with_type(
                |parent| {
                    Operator::Apply.to_document(
                        parent,
                        |parent| function.to_document(parent, annotation),
                        argument,
                        annotation,
                    )
                },
                typ,
                parent,
                annotation,
            ),
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
                    Operator::Arrow.to_document(
                        parent,
                        |parent| {
                            binding
                                .variable_value
                                .type_to_document(parent, Annotation::Off)
                        },
                        &binding.in_expression,
                        Annotation::Off,
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

fn parenthesize_if(condition: bool, docs: impl IntoIterator<Item = Document>) -> Document {
    if condition {
        Document::concat([
            Document::text("("),
            Document::concat(docs),
            Document::text(")"),
        ])
    } else {
        Document::concat(docs)
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
        left: impl FnOnce(Option<(Operator, Side)>) -> Document,
        right: &(impl ?Sized + Pretty),
        annotation: Annotation,
    ) -> Document {
        parenthesize_if(
            self.parenthesize(parent),
            [
                left(Some((self, Side::Left))),
                Document::text(match self {
                    Self::Equals => " = ",
                    Self::HasType => " : ",
                    Self::Arrow => " → ",
                    Self::Apply => " ",
                }),
                right.to_document(Some((self, Side::Right)), annotation),
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
