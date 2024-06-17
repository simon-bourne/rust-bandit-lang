// TODO: Deny unused
#![allow(unused)]
use std::{
    cell::{RefCell, RefMut},
    collections::HashMap,
    rc::Rc,
    result,
};

use bandit_parser::ast::{self, Identifier};

pub struct Program<'src, A: Annotation<'src>> {
    data: Vec<DataDeclaration<'src, A>>,
    values: Vec<Value<'src, A>>,
}

impl<'src> Program<'src, Inference> {
    // TODO: We need an `infer_types` method on `Type` to unify expressions,
    // applications, and their arguments etc. We need to call this method on each
    // type.
    fn infer_types(&mut self, context: &mut Context<'src>) -> Result<()> {
        for d in &mut self.data {
            d.infer_types(context)?;
        }

        for v in &mut self.values {
            v.infer_types(context)?;
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
    fn infer_types(&mut self, context: &mut Context<'src>) -> Result<()> {
        for param in &mut self.parameters {
            param.0.infer_types(context)?;
        }

        Ok(())
    }
}

pub trait Annotation<'src> {
    type Type;
}

struct Inference;

impl<'src> Annotation<'src> for Inference {
    type Type = RefCell<TypeKnowledge<'src>>;
}

enum TypeKnowledge<'src> {
    Known(Type<'src, Inference>),
    Unknown,
    Link(Rc<InferenceType<'src>>),
}

struct Inferred;

impl<'src> Annotation<'src> for Inferred {
    type Type = Type<'src, Self>;
}

struct Context<'src> {
    types: HashMap<Identifier<'src>, Rc<InferenceType<'src>>>,
}

impl<'src> Context<'src> {
    fn insert(&mut self, id: &Identifier<'src>, typ: &mut Rc<InferenceType<'src>>) -> Result<()> {
        if let Some(existing) = self.types.get_mut(id) {
            Type::unify(existing, typ)?;
        } else {
            self.types.insert(id.clone(), typ.clone());
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

trait TypeReference<'src> {
    fn follow_links(&mut self);

    fn known<'a>(&'a self) -> Option<RefMut<'a, Type<'src, Inference>>>;

    fn replace(&mut self, other: &Self);
}

impl<'src> TypeReference<'src> for Rc<InferenceType<'src>> {
    fn follow_links(&mut self) {
        loop {
            let borrowed = self.borrow();
            let TypeKnowledge::Link(link) = &*borrowed else {
                return;
            };

            let link = link.clone();
            drop(borrowed);
            *self = link;
        }
    }

    fn known<'a>(&'a self) -> Option<RefMut<'a, Type<'src, Inference>>> {
        RefMut::filter_map(self.borrow_mut(), |x| {
            if let TypeKnowledge::Known(known) = x {
                Some(known)
            } else {
                None
            }
        })
        .ok()
    }

    fn replace(&mut self, other: &Self) {
        RefCell::replace(self, TypeKnowledge::Link(other.clone()));
        *self = other.clone();
    }
}

impl<'src> Type<'src, Inference> {
    fn unify(x: &mut Rc<InferenceType<'src>>, y: &mut Rc<InferenceType<'src>>) -> Result<()> {
        x.follow_links();
        y.follow_links();

        if Rc::ptr_eq(x, y) {
            return Ok(());
        }

        let Some(x_ref) = x.known() else {
            x.replace(y);
            return Ok(());
        };

        let Some(y_ref) = y.known() else {
            y.replace(x);
            return Ok(());
        };

        todo!("replace x and y with the newly unified type")
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
    fn infer_types(&mut self, context: &mut Context<'src>) -> Result<()> {
        context.insert(&self.name, &mut self.typ)
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
