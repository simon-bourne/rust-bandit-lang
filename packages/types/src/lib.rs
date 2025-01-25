use std::{
    cell::{RefCell, RefMut},
    fmt::Display,
    rc::Rc,
    result,
};

use ::pretty::RcDoc;
use context::{Context, VariableIndex};

pub mod context;
mod pretty;
pub mod source;

// TODO: Use actual refs, not `Rc`
type SharedMut<T> = Rc<RefCell<T>>;

pub type Result<T> = result::Result<T, InferenceError>;

#[derive(Debug)]
pub struct InferenceError;

pub trait Annotation<'src> {
    type Expression: Pretty;
    type VariableName: 'src + Display;
    type VariableIndex: 'src + Display;
}

pub struct Inference;

impl<'src> Annotation<'src> for Inference {
    type Expression = ExpressionRef<'src>;
    type VariableIndex = VariableIndex<'src>;
    type VariableName = EmptyName;
}

struct Inferred;

impl<'src> Annotation<'src> for Inferred {
    type Expression = Rc<Expression<'src, Self>>;
    type VariableIndex = VariableIndex<'src>;
    type VariableName = EmptyName;
}

pub struct EmptyName;

impl Display for EmptyName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("_")
    }
}

type Document = RcDoc<'static>;

pub trait Pretty {
    fn to_document(&self) -> Document;

    fn type_annotatation(&self, term: Document, parenthesized: bool) -> Document;

    fn to_pretty_string(&self, width: usize) -> String {
        self.to_document().pretty(width).to_string()
    }
}

#[derive(Clone)]
pub struct ExpressionRef<'src>(SharedMut<ExprRefVariants<'src>>);

enum ExprRefVariants<'src> {
    Known(Expression<'src, Inference>),
    Unknown,
    Link(ExpressionRef<'src>),
}

impl<'src> ExpressionRef<'src> {
    fn unknown() -> Self {
        Self(Rc::new(RefCell::new(ExprRefVariants::Unknown)))
    }

    fn type_of_type() -> Self {
        Self::new(Expression::Type)
    }

    fn function_type(argument_type: Self, result_type: Self) -> Self {
        Self::new(Expression::FunctionType(VariableBinding {
            name: EmptyName,
            variable_type: argument_type,
            in_expression: result_type,
        }))
    }

    fn new(typ: Expression<'src, Inference>) -> Self {
        Self(Rc::new(RefCell::new(ExprRefVariants::Known(typ))))
    }

    pub fn infer_types(&mut self, ctx: &mut Context<'src>) -> Result<()> {
        match &mut *self.0.borrow_mut() {
            ExprRefVariants::Known(typ) => typ.infer_types(ctx),
            ExprRefVariants::Unknown => Ok(()),
            ExprRefVariants::Link(target) => target.infer_types(ctx),
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

    fn unify(ctx: &mut Context<'src>, x: &mut Self, y: &mut Self) -> Result<()> {
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
            (Expression::Type, Expression::Type) => {}
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
                Self::unify(ctx, function, function1)?;
                Self::unify(ctx, argument, argument1)?;
                Self::unify(ctx, typ, typ1)?;
            }
            (
                Expression::Let {
                    variable_value,
                    binding,
                },
                Expression::Let {
                    variable_value: variable_value1,
                    binding: binding1,
                },
            ) => {
                Self::unify(ctx, variable_value, variable_value1)?;
                VariableBinding::unify(ctx, binding, binding1)?;
            }
            (Expression::FunctionType(binding0), Expression::FunctionType(binding1)) => {
                VariableBinding::unify(ctx, binding0, binding1)?
            }
            (Expression::Lambda(binding0), Expression::Lambda(binding1)) => {
                VariableBinding::unify(ctx, binding0, binding1)?
            }
            // TODO: Keep track of the max allowed de_bruijn index, so we can assert there's no free
            // varaibles.
            (
                Expression::Variable { index, typ },
                Expression::Variable {
                    index: index1,
                    typ: typ1,
                },
            ) if index == index1 => {
                let mut ctx_type = ctx.lookup_type(*index)?;
                Self::unify(ctx, typ, &mut ctx_type)?;
                Self::unify(ctx, typ1, &mut ctx_type)?
            }
            // It's safer to explicitly ignore each variant
            (Expression::Type, _rhs)
            | (Expression::Apply { .. }, _rhs)
            | (Expression::Let { .. }, _rhs)
            | (Expression::FunctionType(_), _rhs)
            | (Expression::Lambda(_), _rhs)
            | (Expression::Variable { .. }, _rhs) => Err(InferenceError)?,
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

    fn typ(&self, ctx: &Context<'src>) -> Result<Self> {
        match &*self.0.borrow() {
            ExprRefVariants::Known(owned) => owned.typ(ctx),
            ExprRefVariants::Unknown => Ok(Self::type_of_type()),
            ExprRefVariants::Link(target) => target.typ(ctx),
        }
    }
}

enum Expression<'src, A: Annotation<'src>> {
    Type,
    Apply {
        function: A::Expression,
        argument: A::Expression,
        typ: A::Expression,
    },
    Let {
        variable_value: A::Expression,
        binding: VariableBinding<'src, A>,
    },
    FunctionType(VariableBinding<'src, A>),
    Lambda(VariableBinding<'src, A>),
    Variable {
        index: A::VariableIndex,
        typ: A::Expression,
    },
}

struct VariableBinding<'src, A: Annotation<'src>> {
    name: A::VariableName,
    variable_type: A::Expression,
    in_expression: A::Expression,
}

impl<'src> VariableBinding<'src, Inference> {
    fn infer_types(&mut self, ctx: &mut Context<'src>) -> Result<()> {
        self.variable_type.infer_types(ctx)?;
        ctx.with_variable(self.variable_type.clone(), |ctx| {
            self.in_expression.infer_types(ctx)
        })
    }

    fn unify(
        ctx: &mut Context<'src>,
        binding0: &mut VariableBinding<'src, Inference>,
        binding1: &mut VariableBinding<'src, Inference>,
    ) -> Result<()> {
        ExpressionRef::unify(
            ctx,
            &mut binding0.variable_type,
            &mut binding1.variable_type,
        )?;

        ctx.with_variable(binding0.variable_type.clone(), |ctx| {
            ExpressionRef::unify(
                ctx,
                &mut binding0.in_expression,
                &mut binding1.in_expression,
            )
        })
    }
}

impl<'src> Expression<'src, Inference> {
    fn infer_types(&mut self, ctx: &mut Context<'src>) -> Result<()> {
        match self {
            Self::Type => (),
            Self::Variable { index, typ } => {
                ExpressionRef::unify(ctx, typ, &mut ctx.lookup_type(*index)?)?;
                typ.infer_types(ctx)?
            }
            Self::Apply {
                function,
                argument,
                typ,
            } => {
                let function_type =
                    &mut ExpressionRef::function_type(argument.typ(ctx)?, typ.clone());
                ExpressionRef::unify(ctx, function_type, &mut function.typ(ctx)?)?;

                function.infer_types(ctx)?;
                argument.infer_types(ctx)?;
                typ.infer_types(ctx)?;
            }
            Self::Let {
                variable_value,
                binding,
            } => {
                variable_value.infer_types(ctx)?;
                binding.infer_types(ctx)?;
            }
            Self::FunctionType(binding) => binding.infer_types(ctx)?,
            Self::Lambda(binding) => binding.infer_types(ctx)?,
        }

        Ok(())
    }

    fn normalize(&mut self) -> Result<()> {
        // TODO: Normalize by evaluation
        Ok(())
    }

    fn typ(&self, ctx: &Context<'src>) -> Result<ExpressionRef<'src>> {
        Ok(match self {
            Self::Type => ExpressionRef::type_of_type(),
            Self::Variable { typ, .. } => typ.clone(),
            Self::Apply { typ, .. } => typ.clone(),
            Self::Let { binding, .. } => binding.in_expression.typ(ctx)?,
            Self::FunctionType(_binding) => ExpressionRef::type_of_type(),
            Self::Lambda(binding) => ExpressionRef::function_type(
                binding.variable_type.clone(),
                binding.in_expression.typ(ctx)?,
            ),
        })
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{context::Context, source::SourceExpression as Expr, Pretty};

    #[test]
    fn infer_kinds() {
        // data X m a = C : (m a) -> X

        // TODO: `C : (m a) -> X`, not `C : (m a)`
        let m = Expr::variable("m", Expr::unknown());
        let a = Expr::variable("a", Expr::unknown());
        let mut constructor_type = Expr::lambda(
            "m",
            Expr::unknown(),
            Expr::lambda("a", Expr::unknown(), Expr::apply(m, a, Expr::unknown())),
        )
        .to_infer()
        .unwrap();

        let ctx = &mut Context::new(HashMap::new());
        constructor_type.infer_types(ctx).unwrap();
        assert_eq!(
            constructor_type.to_pretty_string(80),
            "(\\_:({unknown} -> {unknown}) = (\\_ = ((2:({unknown} -> {unknown})) 1)))"
        );
    }

    #[test]
    fn let_error() {
        // let x : Int = x : Float
        let int_type = Expr::variable("Int", Expr::type_of_type());
        let float_type = Expr::variable("Float", Expr::type_of_type());
        let one = Expr::variable("one", int_type.clone());
        let let_binding =
            Expr::let_binding("x", int_type.clone(), one, Expr::variable("x", float_type));

        let mut global_types = HashMap::new();
        global_types.insert("one", int_type.to_infer().unwrap());
        global_types.insert("Int", Expr::type_of_type().to_infer().unwrap());
        global_types.insert("Float", Expr::type_of_type().to_infer().unwrap());
        let ctx = &mut Context::new(global_types);

        assert!(let_binding.to_infer().unwrap().infer_types(ctx).is_err());
    }
}
