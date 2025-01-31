use super::{ExprRefVariants, ExpressionRef, VariableReference};
use crate::{
    pretty::{variable_to_document, Annotation, Document, Operator, Side},
    Pretty,
};

impl Pretty for ExpressionRef<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotation: Annotation) -> Document {
        match &*self.0.borrow() {
            ExprRefVariants::Known { expression } => expression.to_document(parent, annotation),
            ExprRefVariants::Unknown { typ } => "_".annotate_with_type(typ, parent, annotation),
            ExprRefVariants::Link { target } => target.to_document(parent, annotation),
        }
    }

    fn type_to_document(&self, parent: Option<(Operator, Side)>) -> Document {
        self.typ().to_document(parent, Annotation::Off)
    }

    fn is_known(&self) -> bool {
        match &*self.0.borrow() {
            ExprRefVariants::Known { .. } => true,
            ExprRefVariants::Unknown { .. } => false,
            ExprRefVariants::Link { target } => target.is_known(),
        }
    }

    fn type_is_known(&self) -> bool {
        match &*self.0.borrow() {
            ExprRefVariants::Known { expression } => expression.type_is_known(),
            ExprRefVariants::Unknown { typ } => typ.is_known(),
            ExprRefVariants::Link { target } => target.type_is_known(),
        }
    }
}

impl Pretty for VariableReference<'_> {
    fn to_document(&self, parent: Option<(Operator, Side)>, annotation: Annotation) -> Document {
        variable_to_document(self.name, &self.value, parent, annotation)
    }

    fn type_to_document(&self, parent: Option<(Operator, Side)>) -> Document {
        self.value.type_to_document(parent)
    }

    fn is_known(&self) -> bool {
        self.name.is_known()
    }

    fn type_is_known(&self) -> bool {
        self.value.type_is_known()
    }
}
