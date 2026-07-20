use std::{
    cell::{BorrowMutError, Ref, RefCell, RefMut},
    error::Error,
    fmt,
    rc::Rc,
    result,
};

pub mod ast;
pub mod constraints;
pub mod context;
mod pretty;
mod sync;
pub mod typed;

pub use pretty::Pretty;

#[derive(Default)]
struct SharedMut<T>(Rc<RefCell<T>>);

impl<T> SharedMut<T> {
    pub fn new(x: T) -> Self {
        Self(Rc::new(RefCell::new(x)))
    }

    pub fn is_same(x: &Self, y: &Self) -> bool {
        Rc::ptr_eq(&x.0, &y.0)
    }

    pub fn borrow(&self) -> Ref<'_, T> {
        self.0.borrow()
    }

    pub fn borrow_mut(&self) -> RefMut<'_, T> {
        self.0.borrow_mut()
    }

    pub fn try_borrow_mut(&self) -> result::Result<RefMut<'_, T>, BorrowMutError> {
        self.0.try_borrow_mut()
    }

    pub fn replace_with(&self, x: T) -> T {
        RefCell::replace(self.0.as_ref(), x)
    }

    pub fn id(&self) -> impl fmt::Debug {
        Rc::as_ptr(&self.0)
    }
}

impl<T> Clone for SharedMut<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

#[derive(Debug)]
pub struct ErrorContext<Error, Context> {
    error: Error,
    context: Vec<Context>,
}

impl<E, C> ErrorContext<E, C> {
    fn new(error: E) -> Self {
        Self {
            error,
            context: Vec::new(),
        }
    }
}

impl<E, C> Error for ErrorContext<E, C>
where
    E: fmt::Debug + fmt::Display,
    C: fmt::Debug + fmt::Display,
{
}

impl<E, C> fmt::Display for ErrorContext<E, C>
where
    E: fmt::Display,
    C: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.error)?;

        for context in &self.context {
            writeln!(f, "Context: {context}")?;
        }

        Ok(())
    }
}

pub trait AddErrorContext<T, C> {
    fn context(self, ctx: C) -> Self;
}

#[derive(Debug)]
pub struct InferenceErrorContext;

impl fmt::Display for InferenceErrorContext {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

pub type Result<T> = result::Result<T, ErrorContext<InferenceError, InferenceErrorContext>>;

impl<T> AddErrorContext<InferenceError, InferenceErrorContext> for Result<T> {
    fn context(self, ctx: InferenceErrorContext) -> Self {
        self.map_err(|mut e| {
            e.context.push(ctx);
            e
        })
    }
}

// TODO: Better error handling.
#[derive(Debug)]
pub enum InferenceError {
    CouldntInferAllTypes,
    UnexpectedTypeDuringEval,
    CouldntUnify,
    InfiniteTerm,
    VariableNotFound,
    TopLevelCircularDependency,
    OutOfScope,
}

impl InferenceError {
    pub fn couldnt_infer_all_types() -> ErrorContext<Self, InferenceErrorContext> {
        ErrorContext::new(InferenceError::CouldntInferAllTypes)
    }

    pub fn unexpected_type_during_eval() -> ErrorContext<Self, InferenceErrorContext> {
        ErrorContext::new(InferenceError::UnexpectedTypeDuringEval)
    }

    pub fn couldnt_unify() -> ErrorContext<Self, InferenceErrorContext> {
        ErrorContext::new(InferenceError::CouldntUnify)
    }

    pub fn infinite_term() -> ErrorContext<Self, InferenceErrorContext> {
        ErrorContext::new(InferenceError::InfiniteTerm)
    }

    pub fn variable_not_found() -> ErrorContext<Self, InferenceErrorContext> {
        ErrorContext::new(InferenceError::VariableNotFound)
    }

    pub fn top_level_circular_dependency() -> ErrorContext<Self, InferenceErrorContext> {
        ErrorContext::new(InferenceError::TopLevelCircularDependency)
    }

    pub fn out_of_scope() -> ErrorContext<Self, InferenceErrorContext> {
        ErrorContext::new(InferenceError::OutOfScope)
    }
}

impl fmt::Display for InferenceError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            InferenceError::CouldntInferAllTypes => "CouldntInferAllTypes",
            InferenceError::UnexpectedTypeDuringEval => "UnexpectedTypeDuringEval",
            InferenceError::CouldntUnify => "CouldntUnify",
            InferenceError::InfiniteTerm => "InfiniteTerm",
            InferenceError::VariableNotFound => "VariableNotFound",
            InferenceError::TopLevelCircularDependency => "TopLevelCircularDependency",
            InferenceError::OutOfScope => "OutOfScope",
        })
    }
}

impl Error for InferenceError {}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum ArgumentStyle {
    Implicit,
    Explicit,
}

trait Variable {
    type Declaration;
}

struct VariableBinding<Term: Variable, Discriminator> {
    variable: <Term as Variable>::Declaration,
    in_term: Term,
    discriminator: Discriminator,
}
