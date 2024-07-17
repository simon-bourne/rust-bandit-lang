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
    Constructor(TypeConstructor<'src, A>),
    Apply {
        left: A::Type,
        right: A::Type,
        typ: A::Type,
    },
    Variable {
        name: ast::Identifier<'src>,
        typ: A::Type,
    },
}

impl<'src> Type<'src, Inference> {
    fn infer_types(&mut self, context: &mut Context<'src>) -> Result<()> {
        match self {
            Self::Base => (),
            Self::Constructor(constructor) => {
                if let TypeConstructor::Named { name, typ } = constructor {
                    context.insert(name, typ)?;
                }
            }
            Self::Apply { left, right, typ } => {
                let mut a = TypeKnowledge::new_unknown();
                let mut b = TypeKnowledge::new_unknown();
                let mut f = TypeConstructor::arrow(a.clone(), b.clone());
                Self::unify(&mut f, left)?;
                Self::unify(&mut a, right)?;
                Self::unify(&mut b, typ)?;
            }
            Self::Variable { name, typ } => context.insert(name, typ)?,
        }

        Ok(())
    }

    fn typ(&self) -> InferenceType<'src> {
        match self {
            Self::Base => Self::type_of_type(),
            Self::Constructor(cons) => cons.typ(),
            Self::Apply { typ, .. } => typ.clone(),
            Self::Variable { typ, .. } => typ.clone(),
        }
    }

    fn type_of_type() -> InferenceType<'src> {
        TypeKnowledge::new_known(Self::Base)
    }

    fn unify(x: &mut InferenceType<'src>, y: &mut InferenceType<'src>) -> Result<()> {
        x.follow_links();
        y.follow_links();

        if Rc::ptr_eq(x, y) {
            return Ok(());
        }

        let Some(mut x_ref) = x.known() else {
            x.replace(y);
            return Ok(());
        };

        let Some(mut y_ref) = y.known() else {
            y.replace(x);
            return Ok(());
        };

        // TODO: Can we use mutable borrowing to do the occurs check for us?
        match (&mut *x_ref, &mut *y_ref) {
            (Type::Base, Type::Base) => (),
            (Type::Constructor(c1), Type::Constructor(c2)) => TypeConstructor::unify(c1, c2)?,
            (
                Type::Apply { left, right, typ },
                Type::Apply {
                    left: left1,
                    right: right1,
                    typ: typ1,
                },
            ) => {
                Self::unify(left, left1)?;
                Self::unify(right, right1)?;
                Self::unify(typ, typ1)?;
            }
            _ => Err(InferenceError)?,
        }

        drop(x_ref);
        drop(y_ref);
        *x = y.clone();

        Ok(())
    }
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

enum TypeConstructor<'src, A: Annotation<'src>> {
    Arrow,
    Named {
        name: ast::Identifier<'src>,
        typ: A::Type,
    },
}

impl<'src> TypeConstructor<'src, Inference> {
    fn arrow(left: InferenceType<'src>, right: InferenceType<'src>) -> InferenceType<'src> {
        TypeKnowledge::new_known(Type::Apply {
            left,
            right,
            typ: Self::arrow_type(),
        })
    }

    fn arrow_type() -> InferenceType<'src> {
        Self::arrow(
            Type::type_of_type(),
            Self::arrow(Type::type_of_type(), Type::type_of_type()),
        )
    }

    fn typ(&self) -> InferenceType<'src> {
        match self {
            Self::Arrow => Self::arrow_type(),
            Self::Named { name, typ } => typ.clone(),
        }
    }

    fn unify(left: &mut Self, right: &mut Self) -> Result<()> {
        match (left, right) {
            (Self::Arrow, Self::Arrow) => (),
            (
                Self::Named { name, typ },
                Self::Named {
                    name: name1,
                    typ: typ1,
                },
            ) => {
                if name != name1 {
                    Err(InferenceError)?;
                }

                Type::unify(typ, typ1)?;
            }
            _ => Err(InferenceError)?,
        }

        Ok(())
    }
}

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
