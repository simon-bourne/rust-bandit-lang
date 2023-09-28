struct EffectfulState {
    current_continuation: Option<fn(&mut EffectfulState)>,
}

fn effectful_enter_try(state: &mut EffectfulState) {
    println!("Enter try");
    state.current_continuation = Some(effectful_after_throw);
}

fn effectful_after_throw(_state: &mut EffectfulState) {
    panic!("After throw");
}

fn handle_exceptions(state: &mut EffectfulState) {}

fn main() {
    println!("Hello world")
}
