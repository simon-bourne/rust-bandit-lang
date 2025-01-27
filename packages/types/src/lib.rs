use std::{
    cell::{RefCell, RefMut},
    fmt::Display,
    rc::Rc,
    result,
};

use ::pretty::RcDoc;
use context::{Context, VariableReference};

pub mod context;
mod pretty;
pub mod source;

pub use pretty::Pretty;

type SharedMut<T> = Rc<RefCell<T>>;

pub type Result<T> = result::Result<T, InferenceError>;

#[derive(Debug)]
pub struct InferenceError;

pub trait Annotation<'src> {
    type Expression: Pretty;
    type VariableName: 'src + Display;
    type VariableIndex: 'src + VariableIndex;
}

pub trait VariableIndex: Display {
    fn is_infix(&self) -> bool;
}

pub struct Inference;

impl<'src> Annotation<'src> for Inference {
    type Expression = ExpressionRef<'src>;
    type VariableIndex = VariableReference<'src>;
    type VariableName = EmptyName;
}

struct Inferred;

impl<'src> Annotation<'src> for Inferred {
    type Expression = Rc<Expression<'src, Self>>;
    type VariableIndex = VariableReference<'src>;
    type VariableName = EmptyName;
}

pub struct EmptyName;

impl Display for EmptyName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("_")
    }
}

type Document = RcDoc<'static>;

#[derive(Copy, Clone)]
pub enum TypeAnnotations {
    On,
    Off,
}

#[derive(Copy, Clone)]
pub enum Parentheses {
    On,
    Off,
}

#[derive(Clone)]
pub struct ExpressionRef<'src>(SharedMut<ExprRefVariants<'src>>);

enum ExprRefVariants<'src> {
    Known {
        expression: Expression<'src, Inference>,
    },
    Unknown {
        typ: ExpressionRef<'src>,
    },
    Link {
        target: ExpressionRef<'src>,
    },
}

impl<'src> ExpressionRef<'src> {
    fn unknown(typ: Self) -> Self {
        Self(Rc::new(RefCell::new(ExprRefVariants::Unknown { typ })))
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

    fn new(expression: Expression<'src, Inference>) -> Self {
        Self(Rc::new(RefCell::new(ExprRefVariants::Known { expression })))
    }

    /// Apply type annotations and infer types.
    pub fn normalize(&mut self, ctx: &mut Context<'src>) -> Result<()> {
        self.apply_annotations(ctx)?;
        self.infer_types(ctx)
    }

    fn infer_types(&mut self, ctx: &mut Context<'src>) -> Result<()> {
        match &mut *self.0.borrow_mut() {
            ExprRefVariants::Known { expression } => expression.infer_types(ctx),
            ExprRefVariants::Unknown { typ } => typ.infer_types(ctx),
            ExprRefVariants::Link { target } => target.infer_types(ctx),
        }
    }

    fn apply_annotations(&mut self, ctx: &mut Context<'src>) -> Result<()> {
        let annotated_term = match &mut *self.0.borrow_mut() {
            ExprRefVariants::Known { expression } => expression.apply_annotations(ctx)?,
            ExprRefVariants::Unknown { typ } => {
                typ.apply_annotations(ctx)?;
                None
            }
            ExprRefVariants::Link { target } => {
                target.apply_annotations(ctx)?;
                None
            }
        };

        if let Some(annotated_term) = annotated_term {
            self.replace(&annotated_term);
        }

        Ok(())
    }

    fn map_known<Output>(
        &self,
        f: impl FnOnce(&Expression<'src, Inference>) -> Output,
    ) -> Option<Output> {
        match &*self.0.borrow() {
            ExprRefVariants::Known { expression } => Some(f(expression)),
            ExprRefVariants::Unknown { .. } => None,
            ExprRefVariants::Link { target } => target.map_known(f),
        }
    }

    fn follow_links(&mut self) {
        loop {
            let borrowed = self.0.borrow();
            let ExprRefVariants::Link { target } = &*borrowed else {
                return;
            };

            let link = target.clone();
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
            Self::unify(ctx, &mut x.typ(ctx)?, &mut y.typ(ctx)?)?;
            x.replace(y);
            return Ok(());
        };

        let Some(mut y_ref) = y.known() else {
            drop(x_ref);
            Self::unify(ctx, &mut x.typ(ctx)?, &mut y.typ(ctx)?)?;
            y.replace(x);
            return Ok(());
        };

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
        RefCell::replace(
            &self.0,
            ExprRefVariants::Link {
                target: other.clone(),
            },
        );
        *self = other.clone();
    }

    fn known<'a>(&'a self) -> Option<RefMut<'a, Expression<'src, Inference>>> {
        RefMut::filter_map(self.0.borrow_mut(), |x| {
            if let ExprRefVariants::Known { expression } = x {
                Some(expression)
            } else {
                None
            }
        })
        .ok()
    }

    fn typ(&self, ctx: &Context<'src>) -> Result<Self> {
        match &*self.0.borrow() {
            ExprRefVariants::Known { expression } => expression.typ(ctx),
            ExprRefVariants::Unknown { typ } => Ok(typ.clone()),
            ExprRefVariants::Link { target } => target.typ(ctx),
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
    // TODO: Do we need to store a constraint on the binding, like `a ~ expression`?
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
    fn apply_annotations(&mut self, ctx: &mut Context<'src>) -> Result<()> {
        self.variable_type.apply_annotations(ctx)?;
        self.in_expression.apply_annotations(ctx)
    }

    fn infer_types(&mut self, ctx: &mut Context<'src>) -> Result<()> {
        ExpressionRef::unify(
            ctx,
            &mut self.variable_type.typ(ctx)?,
            &mut ExpressionRef::type_of_type(),
        )?;
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
    fn apply_annotations(
        &mut self,
        ctx: &mut Context<'src>,
    ) -> Result<Option<ExpressionRef<'src>>> {
        match self {
            Self::Type => (),
            Self::Apply {
                function,
                argument,
                typ,
            } => {
                typ.apply_annotations(ctx)?;

                if let Some(annotated_term) = function
                    .map_known(Expression::get_annotation_term)
                    .flatten()
                {
                    ExpressionRef::unify(ctx, &mut annotated_term.typ(ctx)?, argument)?;
                    return Ok(Some(annotated_term));
                }
            }
            Self::Let {
                variable_value,
                binding,
            } => {
                variable_value.apply_annotations(ctx)?;
                binding.apply_annotations(ctx)?
            }
            Self::FunctionType(variable_binding) => variable_binding.apply_annotations(ctx)?,
            Self::Lambda(variable_binding) => variable_binding.apply_annotations(ctx)?,
            Self::Variable { typ, .. } => typ.apply_annotations(ctx)?,
        }

        Ok(None)
    }

    /// If this is `apply (:) term`, return `Some(term)` else `None`
    fn get_annotation_term(&self) -> Option<ExpressionRef<'src>> {
        match self {
            Self::Apply {
                function, argument, ..
            } => function
                .map_known(|e| e.is_annotation_operator().then(|| argument.clone()))
                .flatten(),
            _ => None,
        }
    }

    fn is_annotation_operator(&self) -> bool {
        matches!(
            self,
            Self::Variable {
                index: VariableReference::Global(":"),
                ..
            }
        )
    }

    fn infer_types(&mut self, ctx: &mut Context<'src>) -> Result<()> {
        match self {
            Self::Type => (),
            Self::Variable { index, typ } => {
                ExpressionRef::unify(ctx, &mut typ.typ(ctx)?, &mut ExpressionRef::type_of_type())?;
                ExpressionRef::unify(ctx, typ, &mut ctx.lookup_type(*index)?)?;
                typ.infer_types(ctx)?
            }
            Self::Apply {
                function,
                argument,
                typ,
            } => {
                ExpressionRef::unify(ctx, &mut typ.typ(ctx)?, &mut ExpressionRef::type_of_type())?;
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
        // C : (m a)
        let m = Expr::variable("m");
        let a = Expr::variable("a");
        let mut constructor_type = Expr::lambda(
            "m",
            Expr::unknown_type(),
            Expr::lambda("a", Expr::unknown_type(), Expr::apply(m, a)),
        )
        .to_infer()
        .unwrap();

        let ctx = &mut Context::new(HashMap::new());
        constructor_type.normalize(ctx).unwrap();
        assert_eq!(
            constructor_type.to_pretty_string(80),
            "(\\_ : (_ → _) ⇒ (\\_ ⇒ ((2 : (_ → _)) 1)))"
        );
    }

    #[test]
    fn let_error() {
        // let x : Int = 1 in x : Float
        let int_type = Expr::variable("Int");
        let float_type = Expr::variable("Float");
        let one = Expr::variable("one").annotate(int_type.clone());
        let let_binding = Expr::let_binding(
            "x",
            int_type.clone(),
            one,
            Expr::variable("x").annotate(float_type),
        );

        let mut global_types = HashMap::new();
        global_types.insert("one", int_type.to_infer().unwrap());
        global_types.insert("Int", Expr::type_of_type().to_infer().unwrap());
        global_types.insert("Float", Expr::type_of_type().to_infer().unwrap());
        let ctx = &mut Context::new(global_types);

        assert!(let_binding.to_infer().unwrap().normalize(ctx).is_err());
    }
}
