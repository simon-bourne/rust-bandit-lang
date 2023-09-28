// The function is compiled to delimited continuations.
// The action enum can hold arguments for any action that can happen.

enum Action {
    Exception(ExceptionAction),
}

enum ExceptionAction {
    Throw(Exception),
}

struct Exception {
    message: String,
}

type Continuation = fn(&mut EffectfulState) -> Option<ActionHandler>;
type ActionHandler = fn(&mut EffectfulState);

pub struct EffectfulState {
    throw_handler: ActionHandler,
    exception_end: Option<Continuation>,
    action_args: Option<Action>,
    continuation: Continuation,
}

impl EffectfulState {
    pub fn new(throw_handler: ActionHandler) -> Self {
        Self {
            throw_handler,
            exception_end: None,
            action_args: None,
            continuation: effectful::enter_try,
        }
    }
}

mod effectful {
    // An effectful function, split into delimited continuations.
    use crate::{Action, ActionHandler, EffectfulState, Exception, ExceptionAction};

    pub fn enter_try(state: &mut EffectfulState) -> Option<ActionHandler> {
        println!("Enter try");
        state.exception_end = Some(after_exception_handler);
        state.action_args = Some(Action::Exception(ExceptionAction::Throw(Exception {
            message: "Testing".to_string(),
        })));
        state.continuation = after_throw;
        Some(state.throw_handler)
    }

    pub fn after_throw(_state: &mut EffectfulState) -> Option<ActionHandler> {
        panic!("After throw");
    }

    pub fn after_exception_handler(_state: &mut EffectfulState) -> Option<ActionHandler> {
        println!("Finished");
        None
    }
}

mod handlers {
    use crate::{Action, EffectfulState, Exception, ExceptionAction};

    pub fn throw(state: &mut EffectfulState) {
        if let Some(Action::Exception(ExceptionAction::Throw(Exception { message }))) =
            &state.action_args
        {
            println!("Exception: {message}")
        } else {
            panic!("Bad arguments");
        }

        // TODO: Unwind the stack.
        state.continuation = state.exception_end.unwrap();
    }
}

fn main() {
    let mut state = EffectfulState::new(handlers::throw);

    while let Some(handler) = (state.continuation)(&mut state) {
        handler(&mut state);
    }
}
