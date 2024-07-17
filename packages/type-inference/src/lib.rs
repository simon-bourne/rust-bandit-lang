// TODO: Deny unused
#![allow(unused)]
use std::{
    cell::{RefCell, RefMut},
    collections::HashMap,
    rc::Rc,
    result,
};

use bandit_parser::ast::{self, Identifier};
use derive_more::Constructor;

pub struct Program<'src, A: Annotation<'src>> {
    data: Vec<DataDeclaration<'src, A>>,
    values: Vec<Value<'src, A>>,
}

impl<'src> Program<'src, Inference> {
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
    type Type = Rc<RefCell<TypeKnowledge<'src>>>;
}

enum TypeKnowledge<'src> {
    Known(Type<'src, Inference>),
    Unknown,
    Link(InferenceType<'src>),
}

impl<'src> TypeKnowledge<'src> {
    fn new_known(typ: Type<'src, Inference>) -> Rc<RefCell<Self>> {
        Self::new_shared(Self::Known(typ))
    }

    fn new_unknown() -> Rc<RefCell<Self>> {
        Self::new_shared(Self::Unknown)
    }

    fn new_shared(self) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(self))
    }

    fn infer_types(&mut self, context: &mut Context<'src>) -> Result<()> {
        match self {
            Self::Known(typ) => typ.infer_types(context),
            Self::Unknown => Ok(()),
            Self::Link(target) => target.borrow_mut().infer_types(context),
        }
    }
}

struct Inferred;

impl<'src> Annotation<'src> for Inferred {
    type Type = Box<Type<'src, Self>>;
}

struct Context<'src> {
    types: HashMap<Identifier<'src>, InferenceType<'src>>,
}

impl<'src> Context<'src> {
    fn insert(&mut self, id: &Identifier<'src>, typ: &mut InferenceType<'src>) -> Result<()> {
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
        implicit: Vec<Parameter<'src, A>>,
        explicit: A::Type,
    },
    Constructor(TypeConstructor<'src, A>),
    Arrow(Arrow<A::Type>),
    Apply(Apply<A::Type>),
}

impl<'src> Type<'src, Inference> {
    fn infer_types(&mut self, context: &mut Context<'src>) -> Result<()> {
        match self {
            Self::Base => (),
            // We don't infer types across quantification boundaries to keep things simple.
            Self::Quantified { .. } => (),
            Self::Constructor(constructor) => {
                constructor.0.typ.borrow_mut().infer_types(context)?
            }
            Self::Arrow(arrow) => {
                arrow.left.borrow_mut().infer_types(context)?;
                arrow.right.borrow_mut().infer_types(context)?
            }
            Self::Apply(apply) => {
                let mut a = TypeKnowledge::new_unknown();
                let mut b = TypeKnowledge::new_unknown();
                let mut f = TypeKnowledge::new_known(Self::Arrow(Arrow::new(a.clone(), b.clone())));
                Self::unify(&mut f, &mut apply.left)?;
                Self::unify(&mut a, &mut apply.right)?;
                Self::unify(&mut b, &mut apply.typ)?;
            }
        }

        Ok(())
    }

    fn typ(&self) -> InferenceType<'src> {
        match self {
            Self::Base | Self::Arrow(_) | Self::Quantified { .. } => {
                TypeKnowledge::new_known(Self::Base)
            }
            Self::Constructor(cons) => cons.0.typ.clone(),
            Self::Apply(apply) => apply.typ.clone(),
        }
    }

    fn unify(x: &mut InferenceType<'src>, y: &mut InferenceType<'src>) -> Result<()> {
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

        // TODO: Can we use mutable borrowing to do the occurs check for us?
        // TODO: Unify the types

        todo!("replace x and y with the newly unified type")
    }
}

#[derive(Constructor)]
struct Arrow<Type> {
    left: Type,
    right: Type,
}

struct Apply<Type> {
    left: Type,
    right: Type,
    typ: Type,
}

type InferenceType<'src> = <Inference as Annotation<'src>>::Type;

trait TypeReference<'src> {
    fn follow_links(&mut self);

    fn known<'a>(&'a self) -> Option<RefMut<'a, Type<'src, Inference>>>;

    fn replace(&mut self, other: &Self);
}

impl<'src> TypeReference<'src> for InferenceType<'src> {
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

struct TypeConstructor<'src, A: Annotation<'src>>(Value<'src, A>);
struct Parameter<'src, A: Annotation<'src>>(Value<'src, A>);

struct Value<'src, A: Annotation<'src>> {
    name: ast::Identifier<'src>,
    typ: A::Type,
}

impl<'src> Value<'src, Inference> {
    fn infer_types(&mut self, context: &mut Context<'src>) -> Result<()> {
        context.insert(&self.name, &mut self.typ)?;
        self.typ.borrow_mut().infer_types(context)
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
