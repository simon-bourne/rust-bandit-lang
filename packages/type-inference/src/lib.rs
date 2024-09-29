// TODO: Deny unused
#![allow(unused)]
use std::{
    cell::{RefCell, RefMut},
    collections::HashMap,
    rc::Rc,
    result,
};

use derive_more::Constructor;
use pretty::RcDoc;
use slotmap::{new_key_type, SlotMap};

type SharedMut<T> = Rc<RefCell<T>>;
type PrettyDoc = RcDoc<'static>;

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

#[derive(Debug)]
pub struct ValueConstructor<'src, A: Annotation<'src>> {
    id: Id,
    typ: A::Type,
}

impl<'src, A> ValueConstructor<'src, A>
where
    A: Annotation<'src>,
{
    fn pretty(&self) -> PrettyDoc {
        PrettyDoc::concat([
            PrettyDoc::text("("),
            PrettyDoc::text("ValueConstructor"),
            PrettyDoc::text(":"),
            self.typ.pretty(),
            PrettyDoc::text(")"),
        ])
    }
}

impl<'src> ValueConstructor<'src, Inference> {
    fn infer_types(&mut self, context: &mut Context<'src>) -> Result<()> {
        self.typ.borrow_mut().infer_types(context)
    }
}

type Result<T> = result::Result<T, InferenceError>;

#[derive(Debug)]
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
    type Type: Pretty;
}

pub trait Pretty {
    fn pretty(&self) -> PrettyDoc;
}

#[derive(Debug)]
struct Inference;

impl<'src> Annotation<'src> for Inference {
    type Type = SharedMut<TypeRef<'src>>;
}

impl<'src> Pretty for SharedMut<TypeRef<'src>> {
    fn pretty(&self) -> PrettyDoc {
        match &*self.borrow() {
            TypeRef::Known(owned) => owned.pretty(),
            TypeRef::Unknown => PrettyDoc::text("<unknown>"),
            TypeRef::Link(linked) => linked.pretty(),
        }
    }
}

#[derive(Debug)]
enum TypeRef<'src> {
    Known(Type<'src, Inference>),
    Unknown,
    Link(InferenceType<'src>),
}

impl<'src> TypeRef<'src> {
    fn new(typ: Type<'src, Inference>) -> SharedMut<Self> {
        Rc::new(RefCell::new(Self::Known(typ)))
    }

    fn unknown() -> SharedMut<Self> {
        Rc::new(RefCell::new(Self::Unknown))
    }

    fn infer_types(&mut self, context: &mut Context<'src>) -> Result<()> {
        match self {
            Self::Known(typ) => typ.infer_types(context),
            Self::Unknown => Ok(()),
            Self::Link(target) => target.borrow_mut().infer_types(context),
        }
    }

    fn type_of_type() -> SharedMut<Self> {
        Self::new(Type::Base)
    }

    fn unify(x: &mut SharedMut<Self>, y: &mut SharedMut<Self>) -> Result<()> {
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
        x.replace(y);

        Ok(())
    }

    fn apply(left: SharedMut<Self>, right: SharedMut<Self>) -> SharedMut<Self> {
        let typ = right.typ();
        Self::new(Type::Apply { left, right, typ })
    }

    fn arrow(left: SharedMut<Self>, right: SharedMut<Self>) -> SharedMut<Self> {
        Self::apply(
            Self::apply(Self::new(Type::Constructor(TypeConstructor::Arrow)), left),
            right,
        )
    }
}

struct Inferred;

impl<'src> Annotation<'src> for Inferred {
    type Type = Rc<Type<'src, Self>>;
}

impl<'src> Pretty for Rc<Type<'src, Inferred>> {
    fn pretty(&self) -> PrettyDoc {
        self.as_ref().pretty()
    }
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
        TypeRef::unify(item, typ)
    }
}

#[derive(Debug)]
enum Type<'src, A: Annotation<'src>> {
    Base,
    Constructor(TypeConstructor<'src, A>),
    Apply {
        left: A::Type,
        right: A::Type,
        typ: A::Type,
    },
    Variable(A::Type),
}

impl<'src, A: Annotation<'src>> Type<'src, A> {
    fn pretty(&self) -> PrettyDoc {
        match self {
            Type::Base => PrettyDoc::text("Type"),
            Type::Constructor(cons) => cons.pretty(),
            Type::Apply { left, right, typ } => PrettyDoc::concat([
                PrettyDoc::text("("),
                PrettyDoc::text("("),
                left.pretty(),
                PrettyDoc::space(),
                right.pretty(),
                PrettyDoc::text(")"),
                PrettyDoc::text(":"),
                typ.pretty(),
                PrettyDoc::text(")"),
            ]),
            Type::Variable(typ) => PrettyDoc::concat([
                PrettyDoc::text("("),
                PrettyDoc::text("variable"),
                PrettyDoc::text(":"),
                typ.pretty(),
                PrettyDoc::text(")"),
            ]),
        }
    }
}

impl<'src> Type<'src, Inference> {
    fn infer_types(&mut self, context: &mut Context<'src>) -> Result<()> {
        match self {
            Self::Base => (),
            Self::Variable(typ) => typ.borrow_mut().infer_types(context)?,
            Self::Constructor(constructor) => {
                if let TypeConstructor::Named { id, typ } = constructor {
                    context.unify(*id, typ)?;
                    typ.borrow_mut().infer_types(context)?;
                }
            }
            Self::Apply { left, right, typ } => {
                // TODO: Why does the stack overflow when this is after:
                // `TypeRef::unify(&mut f, &mut left.typ())`?
                left.borrow_mut().infer_types(context)?;
                right.borrow_mut().infer_types(context)?;
                typ.borrow_mut().infer_types(context)?;

                let mut a = TypeRef::unknown();
                let mut b = TypeRef::unknown();
                let mut f = TypeRef::arrow(a.clone(), b.clone());
                TypeRef::unify(&mut f, &mut left.typ())?;
                TypeRef::unify(&mut a, &mut right.typ())?;
                TypeRef::unify(&mut b, typ)?;
            }
        }

        Ok(())
    }

    fn typ(&self) -> InferenceType<'src> {
        match self {
            Self::Base => TypeRef::type_of_type(),
            Self::Variable(typ) => typ.clone(),
            Self::Constructor(cons) => cons.typ(),
            Self::Apply { left, right, typ } => typ.clone(),
        }
    }
}

type InferenceType<'src> = <Inference as Annotation<'src>>::Type;

trait TypeReference<'src> {
    fn follow_links(&mut self);

    fn known<'a>(&'a self) -> Option<RefMut<'a, Type<'src, Inference>>>;

    fn replace(&mut self, other: &Self);

    fn typ(&self) -> Self;
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

    fn known<'a>(&'a self) -> Option<RefMut<'a, Type<'src, Inference>>> {
        RefMut::filter_map(self.borrow_mut(), |x| {
            if let TypeRef::Known(known) = x {
                Some(known)
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

    fn typ(&self) -> Self {
        match &*self.borrow() {
            TypeRef::Known(owned) => owned.typ(),
            TypeRef::Unknown => {
                // TODO: This can create multiple inference variables where there should only be
                // one.
                TypeRef::unknown()
            }
            TypeRef::Link(target) => target.typ(),
        }
    }
}

#[derive(Debug)]
enum TypeConstructor<'src, A: Annotation<'src>> {
    Arrow,
    Named { id: Id, typ: A::Type },
}

impl<'src, A: Annotation<'src>> TypeConstructor<'src, A> {
    fn pretty(&self) -> PrettyDoc {
        match self {
            TypeConstructor::Arrow => PrettyDoc::text("(->)"),
            TypeConstructor::Named { id, typ } => PrettyDoc::concat([
                PrettyDoc::text("("),
                PrettyDoc::text("TypeConstructor"),
                PrettyDoc::text(":"),
                typ.pretty(),
                PrettyDoc::text(")"),
            ]),
        }
    }
}

impl<'src> TypeConstructor<'src, Inference> {
    fn unify(left: &mut Self, right: &mut Self) -> Result<()> {
        match (left, right) {
            (Self::Arrow, Self::Arrow) => (),
            (Self::Named { id, typ }, Self::Named { id: id1, typ: typ1 }) => {
                if id != id1 {
                    Err(InferenceError)?;
                }

                TypeRef::unify(typ, typ1)?;
            }
            _ => Err(InferenceError)?,
        }

        Ok(())
    }

    fn typ(&self) -> InferenceType<'src> {
        match self {
            Self::Arrow => TypeRef::arrow(
                TypeRef::unknown(),
                TypeRef::arrow(TypeRef::unknown(), TypeRef::unknown()),
            ),
            Self::Named { id, typ } => typ.clone(),
        }
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
    use std::io::Write;

    use goldenfile::Mint;

    use super::*;

    // TODO: This sholuld be removed once we actually use the types
    // Check there are no cycles in the types
    #[test]
    fn build() {
        let _inferred = Type::<'static, Inferred>::Base;
        let _inference = Type::<'static, Inference>::Base;
    }

    #[test]
    fn infer_kinds() {
        // data X m a = C : (m a) -> X
        let m = TypeRef::new(Type::Variable(TypeRef::unknown()));
        let a = TypeRef::new(Type::Variable(TypeRef::unknown()));

        // TODO: `C : (m a) -> X`, not `C : (m a)`
        let constructor_type = TypeRef::apply(m, a);
        let mut types = SlotMap::with_key();
        let cons_id = types.insert(constructor_type.clone());
        let context = &mut Context { types };
        let mut constructor = ValueConstructor {
            id: cons_id,
            typ: constructor_type,
        };

        constructor.infer_types(context).unwrap();

        let mut mint = Mint::new("tests/goldenfiles");
        let mut output = mint.new_goldenfile("infer-kinds.txt").unwrap();
        constructor.pretty().render(80, &mut output).unwrap();
    }
}
