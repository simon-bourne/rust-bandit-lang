// The function is compiled to delimited continuations.
// The action enum can hold arguments for any action that can happen.

enum ActionArgs {
    Exception(ExceptionAction),
}

enum ExceptionAction {
    Throw(Exception),
}

struct Exception {
    message: String,
}

#[derive(Copy, Clone)]
pub struct Continuation(pub fn(&mut EffectfulState) -> Option<(Action, Continuation)>);

impl Continuation {
    pub fn run(self, state: &mut EffectfulState) -> Option<Continuation> {
        (self.0)(state).map(|(action, cont)| (action.0)(state, cont))
    }

    pub fn resume(self, state: &mut EffectfulState) {
        let mut current = self;

        while let Some(next) = current.run(state) {
            current = next;
        }
    }
}

#[derive(Copy, Clone)]
pub struct Action(pub fn(&mut EffectfulState, Continuation) -> Continuation);

pub struct EffectfulState {
    throw_handler: Action,
    exception_end: Option<Continuation>,
    action_args: Option<ActionArgs>,
}

impl EffectfulState {
    pub fn new(throw_handler: Action) -> Self {
        Self {
            throw_handler,
            exception_end: None,
            action_args: None,
        }
    }
}

pub mod effectful {
    // An effectful function, split into delimited continuations.
    use super::{Action, ActionArgs, Continuation, EffectfulState, Exception, ExceptionAction};

    pub fn enter_try(state: &mut EffectfulState) -> Option<(Action, Continuation)> {
        println!("Enter try");
        state.exception_end = Some(Continuation(after_exception_handler));
        state.action_args = Some(ActionArgs::Exception(ExceptionAction::Throw(Exception {
            message: "Testing".to_string(),
        })));
        Some((state.throw_handler, Continuation(after_throw)))
    }

    pub fn after_throw(_state: &mut EffectfulState) -> Option<(Action, Continuation)> {
        panic!("After throw");
    }

    pub fn after_exception_handler(_state: &mut EffectfulState) -> Option<(Action, Continuation)> {
        println!("Finished");
        None
    }
}

pub mod handlers {
    use super::{ActionArgs, Continuation, EffectfulState, Exception, ExceptionAction};

    pub fn throw(state: &mut EffectfulState, _continuation: Continuation) -> Continuation {
        // Handlers methods on a user defined struct, so they can have state.
        // Each action method takes user defined arguments and a continuation.
        // They can either:
        //
        // - discard the continuation (like `throw`, for example).
        // - run the continuation to completion with it's argument, using the `.resume()`.
        //   If the continuation is copyable, they can run it multiple times.
        // - run the continuation in parts using `.run()`.
        //
        // When throw handlers are compiled, they are also compiled into
        // continuations delimited by actions.
        //
        // Examples
        //
        // - Exceptions: Throw just discards the continuation.
        // - Async functions: How do we interleave each newly spawned task?

        if let Some(ActionArgs::Exception(ExceptionAction::Throw(Exception { message }))) =
            &state.action_args
        {
            println!("Exception: {message}")
        } else {
            panic!("Bad arguments");
        }

        // To return a value, set it in `state`.
        // TODO: Support multiple resumes.
        state.exception_end.unwrap()
    }
}
