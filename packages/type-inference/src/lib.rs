use std::{
    cell::{RefCell, RefMut},
    rc::Rc,
    result,
};

use pretty::RcDoc;
use slotmap::{new_key_type, SlotMap};

pub mod context;

// TODO: Use actual refs, not `Rc`
type SharedMut<T> = Rc<RefCell<T>>;
type PrettyDoc = RcDoc<'static>;

#[derive(Debug)]
pub struct ValueConstructor<'src, A: Annotation<'src>> {
    typ: A::Type,
}

impl<'src, A> ValueConstructor<'src, A>
where
    A: Annotation<'src>,
{
    pub fn pretty(&self) -> PrettyDoc {
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
    pub fn infer_types(&mut self, context: &mut Context<'src>) -> Result<()> {
        self.typ.infer_types(context)
    }
}

pub type Result<T> = result::Result<T, InferenceError>;

#[derive(Debug)]
pub struct InferenceError;

pub trait Annotation<'src> {
    type Type: Pretty;
}

pub trait Pretty {
    fn pretty(&self) -> PrettyDoc;
}

#[derive(Debug)]
pub struct Inference;

impl<'src> Annotation<'src> for Inference {
    type Type = ExpressionRef<'src>;
}

impl Pretty for ExpressionRef<'_> {
    fn pretty(&self) -> PrettyDoc {
        match &*self.0.borrow() {
            ExprRefVariants::Known(owned) => owned.pretty(),
            ExprRefVariants::Unknown => PrettyDoc::text("{unknown}"),
            ExprRefVariants::Link(linked) => linked.pretty(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExpressionRef<'src>(SharedMut<ExprRefVariants<'src>>);

#[derive(Debug)]
enum ExprRefVariants<'src> {
    Known(Expression<'src, Inference>),
    Unknown,
    Link(ExpressionRef<'src>),
}

impl<'src> ExpressionRef<'src> {
    pub fn unknown() -> Self {
        Self(Rc::new(RefCell::new(ExprRefVariants::Unknown)))
    }

    pub fn type_of_type() -> Self {
        Self::new(Expression::Type)
    }

    pub fn apply(function: Self, argument: Self, typ: Self) -> Self {
        Self::new(Expression::Apply {
            function,
            argument,
            typ,
        })
    }

    pub fn arrow(left: Self, right: Self) -> Self {
        Self::apply(
            Self::new(Expression::ApplyArrowTo(left)),
            right,
            Self::type_of_type(),
        )
    }

    pub fn type_constructor(cons: TypeConstructor<'src, Inference>) -> Self {
        Self::new(Expression::Constructor(cons))
    }

    pub fn variable(typ: Self) -> Self {
        Self::new(Expression::Variable(typ))
    }

    pub fn forall(variable: Self, in_expression: Self) -> Self {
        Self::new(Expression::Forall {
            variable,
            in_expression,
        })
    }

    fn new(typ: Expression<'src, Inference>) -> Self {
        Self(Rc::new(RefCell::new(ExprRefVariants::Known(typ))))
    }

    fn infer_types(&mut self, context: &mut Context<'src>) -> Result<()> {
        match &mut *self.0.borrow_mut() {
            ExprRefVariants::Known(typ) => typ.infer_types(context),
            ExprRefVariants::Unknown => Ok(()),
            ExprRefVariants::Link(target) => target.infer_types(context),
        }
    }

    fn follow_links(&mut self) {
        loop {
            let borrowed = self.0.borrow();
            let ExprRefVariants::Link(link) = &*borrowed else {
                return;
            };

            let link = link.clone();
            drop(borrowed);
            *self = link;
        }
    }

    fn unify(x: &mut Self, y: &mut Self) -> Result<()> {
        x.follow_links();
        y.follow_links();

        if Rc::ptr_eq(&x.0, &y.0) {
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

        x_ref.normalize_apply_arrow()?;
        y_ref.normalize_apply_arrow()?;

        // TODO: Can we use mutable borrowing to do the occurs check for us?
        match (&mut *x_ref, &mut *y_ref) {
            (Expression::Type, Expression::Type) => (),
            (Expression::Constructor(c1), Expression::Constructor(c2)) => {
                TypeConstructor::unify(c1, c2)?
            }
            (
                Expression::Apply {
                    function,
                    argument,
                    typ,
                },
                Expression::Apply {
                    function: function1,
                    argument: argument1,
                    typ: typ1,
                },
            ) => {
                Self::unify(function, function1)?;
                Self::unify(argument, argument1)?;
                Self::unify(typ, typ1)?;
            }
            (Expression::ApplyArrowTo(argument0), Expression::ApplyArrowTo(argument1)) => {
                Self::unify(argument0, argument1)?;
            }
            (
                Expression::Forall {
                    variable,
                    in_expression,
                },
                Expression::Forall {
                    variable: variable0,
                    in_expression: in_expression0,
                },
            ) => {
                variable0.replace(variable);
                Self::unify(in_expression, in_expression0)?
            }
            (
                Expression::Forall {
                    variable,
                    in_expression,
                },
                _,
            ) => {
                // TODO: Factor this out.
                variable.replace(&Self::unknown());
                drop(y_ref);
                Self::unify(in_expression, y)?;
                drop(x_ref);
                x.replace(y);
                return Ok(());
            }
            (
                _,
                Expression::Forall {
                    variable,
                    in_expression,
                },
            ) => {
                variable.replace(&Self::unknown());
                drop(x_ref);
                Self::unify(in_expression, x)?;
                drop(y_ref);
                y.replace(x);
                return Ok(());
            }
            _ => Err(InferenceError)?,
        }

        drop(x_ref);
        drop(y_ref);
        x.replace(y);

        Ok(())
    }

    fn replace(&mut self, other: &Self) {
        RefCell::replace(&self.0, ExprRefVariants::Link(other.clone()));
        *self = other.clone();
    }

    fn known<'a>(&'a self) -> Option<RefMut<'a, Expression<'src, Inference>>> {
        RefMut::filter_map(self.0.borrow_mut(), |x| {
            if let ExprRefVariants::Known(known) = x {
                Some(known)
            } else {
                None
            }
        })
        .ok()
    }

    fn typ(&self) -> Self {
        match &*self.0.borrow() {
            ExprRefVariants::Known(owned) => owned.typ(),
            ExprRefVariants::Unknown => Self::type_of_type(),
            ExprRefVariants::Link(target) => target.typ(),
        }
    }

    fn is_arrow_operator(&self) -> bool {
        match &*self.0.borrow() {
            ExprRefVariants::Known(expr) => expr.is_arrow_operator(),
            ExprRefVariants::Unknown => false,
            ExprRefVariants::Link(linked) => linked.is_arrow_operator(),
        }
    }
}

struct Inferred;

impl<'src> Annotation<'src> for Inferred {
    type Type = Rc<Expression<'src, Self>>;
}

impl Pretty for Rc<Expression<'_, Inferred>> {
    fn pretty(&self) -> PrettyDoc {
        self.as_ref().pretty()
    }
}

pub struct Context<'src> {
    types: SlotMap<Id, ExpressionRef<'src>>,
}

impl<'src> Context<'src> {
    fn unify(&mut self, id: Id, typ: &mut ExpressionRef<'src>) -> Result<()> {
        let Some(item) = self.types.get_mut(id) else {
            return Err(InferenceError);
        };

        // TODO: Instantiate `item` with fresh variables.
        ExpressionRef::unify(item, typ)
    }
}

#[derive(Debug)]
enum Expression<'src, A: Annotation<'src>> {
    Type,
    Constructor(TypeConstructor<'src, A>),
    Apply {
        function: A::Type,
        argument: A::Type,
        typ: A::Type,
    },
    // Apply `->` to it's first argument. This is required because `(Type ->)` has type ` Type ->
    // Type`, and `Type -> Type` has `(Type ->)` as a sub expression.
    ApplyArrowTo(A::Type),
    Variable(A::Type),
    Forall {
        variable: A::Type,
        in_expression: A::Type,
    },
}

impl<'src, A: Annotation<'src>> Expression<'src, A> {
    fn pretty(&self) -> PrettyDoc {
        match self {
            Self::Type => PrettyDoc::text("Type"),
            Self::Constructor(cons) => cons.pretty(),
            Self::Apply {
                function: left,
                argument: right,
                typ,
            } => PrettyDoc::concat([
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
            Self::ApplyArrowTo(argument) => {
                PrettyDoc::concat([argument.pretty(), PrettyDoc::text(" ->")])
            }
            Self::Variable(typ) => PrettyDoc::concat([
                PrettyDoc::text("("),
                PrettyDoc::text("variable"),
                PrettyDoc::text(":"),
                typ.pretty(),
                PrettyDoc::text(")"),
            ]),
            Self::Forall { in_expression, .. } => PrettyDoc::concat([
                PrettyDoc::text("("),
                PrettyDoc::text("forall variable. "),
                in_expression.pretty(),
                PrettyDoc::text(")"),
            ]),
        }
    }

    fn is_arrow_operator(&self) -> bool {
        matches!(self, Self::Constructor(TypeConstructor::Arrow))
    }
}

impl<'src> Expression<'src, Inference> {
    fn infer_types(&mut self, context: &mut Context<'src>) -> Result<()> {
        match self {
            Self::Type => (),
            Self::Variable(typ) => typ.infer_types(context)?,
            Self::Constructor(constructor) => {
                if let TypeConstructor::Named { id, typ } = constructor {
                    context.unify(*id, typ)?;
                    typ.infer_types(context)?;
                }
            }
            Self::Apply {
                function,
                argument,
                typ,
            } => {
                let function_type = &mut ExpressionRef::arrow(argument.typ(), typ.clone());
                ExpressionRef::unify(function_type, &mut function.typ())?;

                function.infer_types(context)?;
                argument.infer_types(context)?;
                typ.infer_types(context)?;
            }
            Self::ApplyArrowTo(argument) => {
                ExpressionRef::unify(&mut argument.typ(), &mut ExpressionRef::type_of_type())?;
                argument.infer_types(context)?;
            }
            Self::Forall { in_expression, .. } => {
                in_expression.infer_types(context)?;
            }
        }

        Ok(())
    }

    fn normalize_apply_arrow(&mut self) -> Result<()> {
        if let Self::Apply {
            function,
            argument,
            typ,
        } = self
        {
            if function.is_arrow_operator() {
                ExpressionRef::unify(
                    typ,
                    &mut ExpressionRef::arrow(
                        ExpressionRef::type_of_type(),
                        ExpressionRef::type_of_type(),
                    ),
                )?;
                *self = Self::ApplyArrowTo(argument.clone())
            }
        }

        Ok(())
    }

    fn typ(&self) -> ExpressionRef<'src> {
        match self {
            Self::Type => ExpressionRef::type_of_type(),
            Self::Variable(typ) => typ.clone(),
            Self::Constructor(cons) => cons.typ(),
            Self::Apply { typ, .. } => typ.clone(),
            Self::ApplyArrowTo(_) => {
                ExpressionRef::arrow(ExpressionRef::type_of_type(), ExpressionRef::type_of_type())
            }
            Self::Forall { in_expression, .. } => in_expression.typ(),
        }
    }
}

#[derive(Debug)]
pub enum TypeConstructor<'src, A: Annotation<'src>> {
    Arrow,
    Named { id: Id, typ: A::Type },
}

impl<'src, A: Annotation<'src>> TypeConstructor<'src, A> {
    fn pretty(&self) -> PrettyDoc {
        match self {
            TypeConstructor::Arrow => PrettyDoc::text("(->)"),
            TypeConstructor::Named { typ, .. } => PrettyDoc::concat([
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

                ExpressionRef::unify(typ, typ1)?;
            }
            _ => Err(InferenceError)?,
        }

        Ok(())
    }

    fn typ(&self) -> ExpressionRef<'src> {
        match self {
            Self::Arrow => ExpressionRef::arrow(
                ExpressionRef::type_of_type(),
                ExpressionRef::arrow(ExpressionRef::type_of_type(), ExpressionRef::type_of_type()),
            ),
            Self::Named { typ, .. } => typ.clone(),
        }
    }
}

new_key_type! {
    pub struct Id;
}

#[cfg(test)]
mod tests {
    use goldenfile::Mint;

    use super::*;

    // TODO: This sholuld be removed once we actually use the types
    // Check there are no cycles in the types
    #[test]
    fn build() {
        let _inferred = Expression::<'static, Inferred>::Type;
        let _inference = Expression::<'static, Inference>::Type;
    }

    #[test]
    fn infer_kinds() {
        // data X m a = C : (m a) -> X
        let m = ExpressionRef::variable(ExpressionRef::unknown());
        let a = ExpressionRef::variable(ExpressionRef::unknown());

        // TODO: `C : (m a) -> X`, not `C : (m a)`
        let constructor_type = ExpressionRef::apply(m, a, ExpressionRef::unknown());
        let types = SlotMap::with_key();
        let context = &mut Context { types };
        let mut constructor = ValueConstructor {
            typ: constructor_type,
        };

        constructor.infer_types(context).unwrap();

        let mut mint = Mint::new("tests/goldenfiles");
        let mut output = mint.new_goldenfile("infer-kinds.txt").unwrap();
        constructor.pretty().render(80, &mut output).unwrap();
    }
}
