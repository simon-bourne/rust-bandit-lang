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
pub struct ErrorContext<Error, Context>(Box<ErrorContextData<Error, Context>>);

#[derive(Debug)]
struct ErrorContextData<Error, Context> {
    error: Error,
    context: Vec<Context>,
}

impl<E, C> ErrorContext<E, C> {
    pub fn new(error: E) -> Self {
        Self(Box::new(ErrorContextData {
            error,
            context: Vec::new(),
        }))
    }

    pub fn context(mut self, ctx: C) -> Self {
        self.0.context.push(ctx);
        self
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
        writeln!(f, "{}", self.0.error)?;

        for context in &self.0.context {
            writeln!(f, "Context: {context}")?;
        }

        Ok(())
    }
}

pub trait AddInferenceErrorContext {
    fn when_unifying(self) -> Self;
}

impl<T> AddInferenceErrorContext for Result<T> {
    fn when_unifying(self) -> Self {
        self.map_err(|e| e.context(InferenceErrorContext))
    }
}

#[derive(Debug)]
pub struct InferenceErrorContext;

impl fmt::Display for InferenceErrorContext {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

pub type Result<T> = result::Result<T, ErrorContext<InferenceErrorKind, InferenceErrorContext>>;

// TODO: Better error handling.
#[derive(Debug)]
pub enum InferenceErrorKind {
    CouldntInferAllTypes,
    UnexpectedTypeDuringEval,
    CouldntUnify,
    InfiniteTerm,
    VariableNotFound,
    TopLevelCircularDependency,
    OutOfScope,
}

impl InferenceErrorKind {
    pub fn result<T>(self) -> Result<T> {
        Err(self.error())
    }

    pub fn error(self) -> ErrorContext<Self, InferenceErrorContext> {
        ErrorContext::new(self)
    }
}

impl fmt::Display for InferenceErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            InferenceErrorKind::CouldntInferAllTypes => "CouldntInferAllTypes",
            InferenceErrorKind::UnexpectedTypeDuringEval => "UnexpectedTypeDuringEval",
            InferenceErrorKind::CouldntUnify => "CouldntUnify",
            InferenceErrorKind::InfiniteTerm => "InfiniteTerm",
            InferenceErrorKind::VariableNotFound => "VariableNotFound",
            InferenceErrorKind::TopLevelCircularDependency => "TopLevelCircularDependency",
            InferenceErrorKind::OutOfScope => "OutOfScope",
        })
    }
}

impl Error for InferenceErrorKind {}

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
