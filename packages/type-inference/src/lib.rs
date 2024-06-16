// TODO: Deny unused
#![allow(unused)]
use std::{cell::RefCell, collections::HashMap, rc::Rc, result};

use bandit_parser::ast::{self, Identifier};

pub struct Program<'src, A: Annotation<'src>> {
    data: Vec<DataDeclaration<'src, A>>,
    values: Vec<Value<'src, A>>,
}

impl<'src> Program<'src, Inference> {
    fn build_context(&mut self, context: &mut Context<'src>) -> Result<()> {
        for d in &mut self.data {
            d.build_context(context)?;
        }

        for v in &mut self.values {
            v.build_context(context)?;
        }

        Ok(())
    }
}

type Result<T> = result::Result<T, InferenceError>;

struct InferenceError;

struct DataDeclaration<'src, A: Annotation<'src>> {
    name: ast::Identifier<'src>,
    parameters: Vec<Parameter<'src, A>>,
}

impl<'src> DataDeclaration<'src, Inference> {
    fn build_context(&mut self, context: &mut Context<'src>) -> Result<()> {
        for param in &mut self.parameters {
            param.0.build_context(context)?;
        }

        Ok(())
    }
}

pub trait Annotation<'src> {
    type Type;
}

struct Inference;

impl<'src> Annotation<'src> for Inference {
    type Type = RefCell<Option<Type<'src, Self>>>;
}

struct Inferred;

impl<'src> Annotation<'src> for Inferred {
    type Type = Type<'src, Self>;
}

struct Context<'src> {
    types: HashMap<Identifier<'src>, Rc<InferenceType<'src>>>,
}

impl<'src> Context<'src> {
    fn insert(&mut self, id: Identifier<'src>, typ: &mut Rc<InferenceType<'src>>) -> Result<()> {
        if let Some(existing) = self.types.get_mut(&id) {
            Type::unify(existing, typ)?;
        } else {
            self.types.insert(id, typ.clone());
        }

        Ok(())
    }
}

enum Type<'src, A: Annotation<'src>> {
    Base,
    /// `∀a b c. a -> b -> c` is a type `(a : Type) -> (b : Type) -> (c : Type)
    /// -> a -> b -> c`, where the first 3 arguments are inferred by the
    /// compiler.
    Quantified {
        inferred: Vec<Parameter<'src, A>>,
        explicit: Box<Self>,
    },
    Constructor(TypeConstructor<'src, A>),
    Arrow(Rc<Arrow<A::Type>>),
    Apply(Rc<Apply<A::Type>>),
}

type InferenceType<'src> = <Inference as Annotation<'src>>::Type;

impl<'src> Type<'src, Inference> {
    // TODO: This isn't quite right. We need to make sure we don't start copying
    // parts of the type, as then we might end up unifying with one copy and not
    // another.
    fn unify(x: &mut Rc<InferenceType<'src>>, y: &mut Rc<InferenceType<'src>>) -> Result<()> {
        if Rc::ptr_eq(x, y) {
            return Ok(());
        }

        if x.borrow().is_none() {
            *x = y.clone();
            return Ok(());
        }

        if y.borrow().is_none() {
            *y = x.clone();
            return Ok(());
        }

        todo!()
    }
}

struct Arrow<Type> {
    left: Type,
    right: Type,
}

struct Apply<Type> {
    left: Type,
    right: Type,
    typ: Type,
}

struct TypeConstructor<'src, A: Annotation<'src>>(Value<'src, A>);
struct Parameter<'src, A: Annotation<'src>>(Value<'src, A>);

struct Value<'src, A: Annotation<'src>> {
    name: ast::Identifier<'src>,
    typ: Rc<A::Type>,
}

impl<'src> Value<'src, Inference> {
    fn build_context(&mut self, context: &mut Context<'src>) -> Result<()> {
        context.insert(self.name.clone(), &mut self.typ)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // TODO: This sholuld be removed once we actually use the types
    // Check there are no cycles in the types
    #[test]
    fn build() {
        let _inferred = Type::<'static, Inferred>::Base;
        let _inference = Type::<'static, Inference>::Base;
    }
}
