use pretty::RcDoc;

use super::{Expression, Stage, VariableBinding};
use crate::Variable;

pub trait Pretty {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotations: Annotation) -> Document;

    fn type_to_document(&self, parent: Option<(Operator, Side)>) -> Document;

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

    fn type_is_known(&self) -> bool;

    fn to_pretty_string(&self, width: usize) -> String {
        self.to_document(None, Annotation::On)
            .pretty(width)
            .to_string()
    }
}

impl Pretty for Variable<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotation: Annotation) -> Document {
        self.name.to_document(parent, annotation)
    }

    fn type_to_document(&self, parent: Option<(Operator, Side)>) -> Document {
        self.name.type_to_document(parent)
    }

    fn is_known(&self) -> bool {
        self.name.is_known()
    }

    fn type_is_known(&self) -> bool {
        false
    }
}

impl<'src, S: Stage<'src>> VariableBinding<'src, S> {
    fn to_document(&self, annotation: Annotation) -> Document {
        Document::concat([
            variable_to_document(self.name, &self.variable_value, None, annotation),
            Document::text(" ⇒ "),
            self.in_expression.to_document(None, annotation),
        ])
    }
}

pub fn variable_to_document(
    name: &str,
    value: &impl Pretty,
    parent: Option<(Operator, Side)>,
    annotation: Annotation,
) -> Document {
    if annotation == Annotation::Off {
        return Document::as_string(name);
    }

    let variable_type = |parent| value.type_to_document(parent);

    let variable = |parent| {
        if value.type_is_known() {
            Operator::HasType.to_document(
                parent,
                |_parent| Document::as_string(name),
                variable_type,
            )
        } else {
            Document::as_string(name)
        }
    };

    if value.is_known() {
        Operator::Equals.to_document(parent, variable, |parent| {
            value.to_document(parent, annotation)
        })
    } else {
        variable(parent)
    }
}

impl<'src, S: Stage<'src>> Pretty for Expression<'src, S> {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotation: Annotation) -> Document {
        match self {
            Self::TypeOfType => Document::text("Type"),
            Self::Constant { name, typ } => name.annotate_with_type(typ, parent, annotation),
            Self::Apply {
                function,
                argument,
                typ,
            } => annotate_with_type(
                |parent| {
                    Operator::Apply.to_document(
                        parent,
                        |parent| function.to_document(parent, annotation),
                        |parent| argument.to_document(parent, annotation),
                    )
                },
                typ,
                parent,
                annotation,
            ),
            Self::Let(binding) => parenthesize_if(
                parent.is_some(),
                [
                    Document::text("let"),
                    Document::space(),
                    binding.to_document(annotation),
                ],
            ),
            Self::FunctionType(binding) => {
                let variable_name = &binding.name;

                if variable_name.is_known() {
                    parenthesize_if(
                        parent.is_some(),
                        [Document::text("∀"), binding.to_document(annotation)],
                    )
                } else {
                    Operator::Arrow.to_document(
                        parent,
                        |parent| binding.variable_value.type_to_document(parent),
                        |parent| binding.in_expression.to_document(parent, Annotation::Off),
                    )
                }
            }
            Self::Lambda(binding) => parenthesize_if(
                parent.is_some(),
                [Document::text("\\"), binding.to_document(annotation)],
            ),
            Self::Variable(var) => var.to_document(parent, annotation),
        }
    }

    fn type_to_document(&self, parent: Option<(Operator, Side)>) -> Document {
        match self {
            Self::TypeOfType => self.to_document(parent, Annotation::Off),
            Self::Constant { typ, .. } => typ.to_document(parent, Annotation::Off),
            Self::Apply { typ, .. } => typ.to_document(parent, Annotation::Off),
            Self::Let(variable_binding)
            | Self::FunctionType(variable_binding)
            | Self::Lambda(variable_binding) => {
                variable_binding.variable_value.type_to_document(parent)
            }
            Self::Variable(variable) => variable.type_to_document(parent),
        }
    }

    fn is_known(&self) -> bool {
        true
    }

    fn type_is_known(&self) -> bool {
        match self {
            Self::TypeOfType => true,
            Self::Constant { typ, .. } | Self::Apply { typ, .. } => typ.is_known(),
            Self::Let(binding) | Self::FunctionType(binding) | Self::Lambda(binding) => {
                binding.variable_value.type_is_known()
            }
            Self::Variable(variable) => variable.type_is_known(),
        }
    }
}

impl Pretty for str {
    fn to_document(&self, _parent: Option<(Operator, Side)>, _annotation: Annotation) -> Document {
        Document::as_string(self)
    }

    fn type_to_document(&self, parent: Option<(Operator, Side)>) -> Document {
        "_".to_document(parent, Annotation::Off)
    }

    fn is_known(&self) -> bool {
        self != "_"
    }

    fn type_is_known(&self) -> bool {
        false
    }
}

impl Pretty for &str {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotation: Annotation) -> Document {
        (*self).to_document(parent, annotation)
    }

    fn type_to_document(&self, parent: Option<(Operator, Side)>) -> Document {
        (*self).type_to_document(parent)
    }

    fn is_known(&self) -> bool {
        (*self).is_known()
    }

    fn type_is_known(&self) -> bool {
        (*self).type_is_known()
    }
}

fn annotate_with_type(
    term: impl FnOnce(Option<(Operator, Side)>) -> Document,
    typ: &(impl ?Sized + Pretty),
    parent: Option<(Operator, Side)>,
    annotation: Annotation,
) -> Document {
    if annotation == Annotation::Off {
        return term(parent);
    }

    Operator::HasType.to_document(parent, term, |parent| {
        typ.to_document(parent, Annotation::Off)
    })
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
        right: impl FnOnce(Option<(Operator, Side)>) -> Document,
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
                right(Some((self, Side::Right))),
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

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Annotation {
    On,
    Off,
}
