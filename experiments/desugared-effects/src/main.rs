enum Action {
    Exception(ExceptionAction),
}

enum ExceptionAction {
    Throw(Exception),
}

struct Exception {
    message: String,
}

#[derive(Default)]
struct EffectfulState {
    current_action: Option<Action>,
    current_continuation: Option<fn(&mut EffectfulState)>,
}

fn effectful_enter_try(state: &mut EffectfulState) {
    println!("Enter try");
    state.current_action = Some(Action::Exception(ExceptionAction::Throw(Exception {
        message: "Testing".to_string(),
    })));
    state.current_continuation = Some(effectful_after_throw);
}

fn effectful_after_throw(_state: &mut EffectfulState) {
    panic!("After throw");
}

fn catch(state: &mut EffectfulState) {
    if let Some(action) = &state.current_action {
        match action {
            Action::Exception(ExceptionAction::Throw(Exception { message })) => {
                println!("Exception: {message}")
            }
        }
    }
}

fn main() {
    let mut state = EffectfulState::default();
    effectful_enter_try(&mut state);
    catch(&mut state);
}
