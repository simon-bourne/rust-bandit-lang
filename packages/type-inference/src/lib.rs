use std::{
    cell::{RefCell, RefMut},
    rc::Rc,
    result,
};

use context::Context;
use pretty::RcDoc;

pub mod context;

// TODO: Use actual refs, not `Rc`
type SharedMut<T> = Rc<RefCell<T>>;
type PrettyDoc = RcDoc<'static>;

#[derive(Debug)]
pub struct ValueConstructor<'src, A: Annotation<'src>> {
    typ: A::Expression,
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
    type Expression: Pretty;
}

pub trait Pretty {
    fn pretty(&self) -> PrettyDoc;
}

#[derive(Debug)]
pub struct Inference;

impl<'src> Annotation<'src> for Inference {
    type Expression = ExpressionRef<'src>;
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

    pub fn function_type(argument_type: Self, result_type: Self) -> Self {
        Self::new(Expression::FunctionType(VariableBinding {
            variable_type: argument_type,
            in_expression: result_type,
        }))
    }

    pub fn lambda(argument_type: Self, in_expression: Self) -> Self {
        Self::new(Expression::Lambda(VariableBinding {
            variable_type: argument_type,
            in_expression,
        }))
    }

    pub fn variable(typ: Self) -> Self {
        Self::new(Expression::Variable(typ))
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

        x_ref.normalize()?;
        y_ref.normalize()?;

        // TODO: Can we use mutable borrowing to do the occurs check for us?
        match (&mut *x_ref, &mut *y_ref) {
            (Expression::Type, Expression::Type) => (),
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
            (Expression::FunctionType(binding0), Expression::FunctionType(binding1)) => {
                VariableBinding::unify(binding0, binding1)?
            }
            (Expression::Lambda(binding0), Expression::Lambda(binding1)) => {
                VariableBinding::unify(binding0, binding1)?
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
}

struct Inferred;

impl<'src> Annotation<'src> for Inferred {
    type Expression = Rc<Expression<'src, Self>>;
}

impl Pretty for Rc<Expression<'_, Inferred>> {
    fn pretty(&self) -> PrettyDoc {
        self.as_ref().pretty()
    }
}

#[derive(Debug)]
enum Expression<'src, A: Annotation<'src>> {
    Type,
    Apply {
        function: A::Expression,
        argument: A::Expression,
        typ: A::Expression,
    },
    FunctionType(VariableBinding<'src, A>),
    Lambda(VariableBinding<'src, A>),
    // TODO: Use debruijn index
    Variable(A::Expression),
}

#[derive(Debug)]
struct VariableBinding<'src, A: Annotation<'src>> {
    variable_type: A::Expression,
    in_expression: A::Expression,
}

impl<'src> VariableBinding<'src, Inference> {
    fn infer_types(&mut self, context: &mut Context<'src>) -> Result<()> {
        self.variable_type.infer_types(context)?;
        self.in_expression.infer_types(context)
    }

    fn unify(
        binding0: &mut VariableBinding<'src, Inference>,
        binding1: &mut VariableBinding<'src, Inference>,
    ) -> Result<()> {
        ExpressionRef::unify(&mut binding0.variable_type, &mut binding1.variable_type)
    }
}

impl<'src, A: Annotation<'src>> Expression<'src, A> {
    fn pretty(&self) -> PrettyDoc {
        match self {
            Self::Type => PrettyDoc::text("Type"),
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
            Self::FunctionType(binding) => PrettyDoc::concat([
                PrettyDoc::text("("),
                binding.variable_type.pretty(),
                PrettyDoc::text(" -> "),
                binding.in_expression.pretty(),
                PrettyDoc::text(")"),
            ]),
            Self::Lambda(binding) => PrettyDoc::concat([
                PrettyDoc::text("("),
                PrettyDoc::text("_"),
                PrettyDoc::text(":"),
                binding.variable_type.pretty(),
                PrettyDoc::text(" -> "),
                binding.in_expression.pretty(),
                PrettyDoc::text(")"),
            ]),
            Self::Variable(typ) => PrettyDoc::concat([
                PrettyDoc::text("("),
                PrettyDoc::text("variable"),
                PrettyDoc::text(":"),
                typ.pretty(),
                PrettyDoc::text(")"),
            ]),
        }
    }
}

impl<'src> Expression<'src, Inference> {
    fn infer_types(&mut self, context: &mut Context<'src>) -> Result<()> {
        match self {
            Self::Type => (),
            Self::Variable(typ) => typ.infer_types(context)?,
            Self::Apply {
                function,
                argument,
                typ,
            } => {
                let function_type = &mut ExpressionRef::function_type(argument.typ(), typ.clone());
                ExpressionRef::unify(function_type, &mut function.typ())?;

                function.infer_types(context)?;
                argument.infer_types(context)?;
                typ.infer_types(context)?;
            }
            Self::FunctionType(binding) => binding.infer_types(context)?,
            Self::Lambda(binding) => binding.infer_types(context)?,
        }

        Ok(())
    }

    fn normalize(&mut self) -> Result<()> {
        // TODO: Normalize by evaluation
        Ok(())
    }

    fn typ(&self) -> ExpressionRef<'src> {
        match self {
            Self::Type => ExpressionRef::type_of_type(),
            Self::Variable(typ) => typ.clone(),
            Self::Apply { typ, .. } => typ.clone(),
            Self::FunctionType(_binding) => ExpressionRef::type_of_type(),
            Self::Lambda(binding) => binding.in_expression.typ(),
        }
    }
}

#[cfg(test)]
mod tests {
    use goldenfile::Mint;

    use super::*;

    #[test]
    fn infer_kinds() {
        // data X m a = C : (m a) -> X
        let m = ExpressionRef::variable(ExpressionRef::unknown());
        let a = ExpressionRef::variable(ExpressionRef::unknown());

        // TODO: `C : (m a) -> X`, not `C : (m a)`
        let constructor_type = ExpressionRef::apply(m, a, ExpressionRef::unknown());
        let context = &mut Context::default();
        let mut constructor = ValueConstructor {
            typ: constructor_type,
        };

        constructor.infer_types(context).unwrap();

        let mut mint = Mint::new("tests/goldenfiles");
        let mut output = mint.new_goldenfile("infer-kinds.txt").unwrap();
        constructor.pretty().render(80, &mut output).unwrap();
    }
}
