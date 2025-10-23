use std::marker::PhantomData;

use crate::{DeBruijnIndex, Evaluation, VariableBinding};

mod pretty;

#[derive(Clone)]
pub struct Term<'src>(PhantomData<&'src ()>);

impl<'src> Term<'src> {
    pub fn type_of_type() -> Self {
        todo!()
    }

    pub fn unknown() -> Self {
        todo!()
    }

    pub fn variable(name: &'src str, index: DeBruijnIndex) -> Self {
        todo!()
    }

    pub fn constant(name: &'src str, value: Self) -> Self {
        todo!()
    }

    pub fn apply(_function: Self, _argument: Self, _evaluation: Evaluation) -> Self {
        todo!()
    }

    pub fn has_type(self, _typ: Self) -> Self {
        todo!()
    }

    pub fn let_binding(value: Self, binding: VariableBinding<'src, Self>) -> Self {
        todo!()
    }

    pub fn pi(binding: VariableBinding<'src, Self>) -> Self {
        todo!()
    }

    pub fn lambda(binding: VariableBinding<'src, Self>) -> Self {
        todo!()
    }

    pub fn infer_type(&self) -> Self {
        todo!()
    }
}
