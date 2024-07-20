// TODO: Deny unused
#![allow(unused)]
use std::{
    cell::{RefCell, RefMut},
    collections::HashMap,
    rc::Rc,
    result,
};

use derive_more::Constructor;
use slotmap::{new_key_type, SlotMap};

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

pub struct Data<'src, A: Annotation<'src>> {
    declaration: DataDeclaration<'src, A>,
    constructors: Vec<ValueConstructor<'src, A>>,
}

impl<'src> Data<'src, Inference> {
    fn infer_types(&mut self, context: &mut Context<'src>) -> Result<()> {
        self.declaration.infer_types(context)?;

        for c in &mut self.constructors {
            c.infer_types(context)?;
        }

        Ok(())
    }
}

pub struct ValueConstructor<'src, A: Annotation<'src>> {
    id: Id,
    typ: A::Type,
}

impl<'src> ValueConstructor<'src, Inference> {
    fn infer_types(&mut self, context: &mut Context<'src>) -> Result<()> {
        self.typ.borrow_mut().infer_types(context)
    }
}

type Result<T> = result::Result<T, InferenceError>;

struct InferenceError;

struct DataDeclaration<'src, A: Annotation<'src>> {
    id: Id,
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
    type Type = Rc<RefCell<TypeRef<'src>>>;
}

enum TypeRef<'src> {
    Own(Type<'src, Inference>),
    Link(InferenceType<'src>),
}

impl<'src> TypeRef<'src> {
    fn new(typ: Type<'src, Inference>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self::Own(typ)))
    }

    fn infer_types(&mut self, context: &mut Context<'src>) -> Result<()> {
        match self {
            Self::Own(typ) => typ.infer_types(context),
            Self::Link(target) => target.borrow_mut().infer_types(context),
        }
    }
}

struct Inferred;

impl<'src> Annotation<'src> for Inferred {
    type Type = Rc<Type<'src, Self>>;
}

struct Context<'src> {
    types: SlotMap<Id, InferenceType<'src>>,
}

impl<'src> Context<'src> {
    fn unify(&mut self, id: Id, typ: &mut InferenceType<'src>) -> Result<()> {
        let Some(item) = self.types.get_mut(id) else {
            return Err(InferenceError);
        };

        // TODO: Instantiate `item` with fresh variables.
        Type::unify(item, typ)
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
    Variable,
}

impl<'src> Type<'src, Inference> {
    fn infer_types(&mut self, context: &mut Context<'src>) -> Result<()> {
        match self {
            Self::Base | Self::Variable => (),
            Self::Constructor(constructor) => {
                if let TypeConstructor::Named { id, typ } = constructor {
                    context.unify(*id, typ)?;
                    typ.borrow_mut().infer_types(context)?;
                }
            }
            Self::Apply { left, right, typ } => {
                let mut a = TypeRef::new(Self::Variable);
                let mut b = TypeRef::new(Self::Variable);
                let mut f = TypeConstructor::arrow(a.clone(), b.clone());
                Self::unify(&mut f, left)?;
                Self::unify(&mut a, right)?;
                Self::unify(&mut b, typ)?;

                left.borrow_mut().infer_types(context)?;
                right.borrow_mut().infer_types(context)?;
                typ.borrow_mut().infer_types(context)?;
            }
        }

        Ok(())
    }

    fn type_of_type() -> InferenceType<'src> {
        TypeRef::new(Self::Base)
    }

    fn unify(x: &mut InferenceType<'src>, y: &mut InferenceType<'src>) -> Result<()> {
        x.follow_links();
        y.follow_links();

        if Rc::ptr_eq(x, y) {
            return Ok(());
        }

        let Some(mut x_ref) = x.non_variable() else {
            x.replace(y);
            return Ok(());
        };

        let Some(mut y_ref) = y.non_variable() else {
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
        x.replace(y);

        Ok(())
    }
}

type InferenceType<'src> = <Inference as Annotation<'src>>::Type;

trait TypeReference<'src> {
    fn follow_links(&mut self);

    fn non_variable<'a>(&'a self) -> Option<RefMut<'a, Type<'src, Inference>>>;

    fn replace(&mut self, other: &Self);
}

impl<'src> TypeReference<'src> for InferenceType<'src> {
    fn follow_links(&mut self) {
        loop {
            let borrowed = self.borrow();
            let TypeRef::Link(link) = &*borrowed else {
                return;
            };

            let link = link.clone();
            drop(borrowed);
            *self = link;
        }
    }

    fn non_variable<'a>(&'a self) -> Option<RefMut<'a, Type<'src, Inference>>> {
        RefMut::filter_map(self.borrow_mut(), |x| {
            if let TypeRef::Own(owned) = x {
                let non_variable = !matches!(owned, Type::Variable);
                non_variable.then_some(owned)
            } else {
                None
            }
        })
        .ok()
    }

    fn replace(&mut self, other: &Self) {
        RefCell::replace(self, TypeRef::Link(other.clone()));
        *self = other.clone();
    }
}

enum TypeConstructor<'src, A: Annotation<'src>> {
    Arrow,
    Named { id: Id, typ: A::Type },
}

impl<'src> TypeConstructor<'src, Inference> {
    fn arrow(left: InferenceType<'src>, right: InferenceType<'src>) -> InferenceType<'src> {
        TypeRef::new(Type::Apply {
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

    fn unify(left: &mut Self, right: &mut Self) -> Result<()> {
        match (left, right) {
            (Self::Arrow, Self::Arrow) => (),
            (Self::Named { id, typ }, Self::Named { id: id1, typ: typ1 }) => {
                if id != id1 {
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
    id: Id,
    typ: A::Type,
}

impl<'src> Value<'src, Inference> {
    fn infer_types(&mut self, context: &mut Context<'src>) -> Result<()> {
        context.unify(self.id, &mut self.typ)?;
        self.typ.borrow_mut().infer_types(context)
    }
}

new_key_type! {
    struct Id;
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

    #[test]
    fn basic() {
        // data X m a = C (m a)
        // TODO: Implement
        let m = TypeRef::new(Type::Variable);
        let a = TypeRef::new(Type::Variable);

        // TODO: Don't use `Type::Variable`
        let constructor_type = TypeRef::new(Type::Variable);
        let mut types = SlotMap::with_key();
        let cons_id = types.insert(constructor_type.clone());
        let context = &mut Context { types };
        let mut constructor = ValueConstructor {
            id: cons_id,
            typ: constructor_type,
        };

        constructor.infer_types(context);
    }
}
