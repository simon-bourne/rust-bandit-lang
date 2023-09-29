use bandit_experiment_desugared_effects::desugared::{
    effectful, handlers, Action, Continuation, EffectfulState,
};

fn main() {
    let mut state = EffectfulState::new(Action(handlers::throw));
    Continuation::new(effectful::enter_try).resume(&mut state);
}
