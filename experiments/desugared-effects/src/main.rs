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
pub struct Continuation(fn(&mut EffectfulState) -> Option<(Action, Continuation)>);

#[derive(Copy, Clone)]
pub struct Action(fn(&mut EffectfulState, Continuation) -> Continuation);

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

mod effectful {
    // An effectful function, split into delimited continuations.
    use crate::{Action, ActionArgs, Continuation, EffectfulState, Exception, ExceptionAction};

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

mod handlers {
    use crate::{ActionArgs, Continuation, EffectfulState, Exception, ExceptionAction};

    pub fn throw(state: &mut EffectfulState, _continuation: Continuation) -> Continuation {
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

fn main() {
    let mut state = EffectfulState::new(Action(handlers::throw));
    let mut continuation = Continuation(effectful::enter_try);

    while let Some((action, cont)) = (continuation.0)(&mut state) {
        continuation = (action.0)(&mut state, cont);
    }
}
