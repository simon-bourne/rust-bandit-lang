use std::collections::HashMap;

use bandit_types::{Pretty, context::Context, source::Term};

#[test]
fn infer_kinds() {
    // C : (m a)
    let m = Term::variable("m");
    let a = Term::variable("a");
    let ctx = &mut Context::new(HashMap::new());
    let constructor_type = Term::lambda(
        "m",
        Term::unknown(),
        Term::lambda("a", Term::unknown(), Term::apply(m, a)),
    )
    .link(ctx)
    .unwrap();

    assert_eq!(
        constructor_type.to_pretty_string(80),
        r"\m : _ → _ ⇒ \a ⇒ (m : _ → _) a"
    );
}

#[test]
fn let_error() {
    // let x : Int = 1 in x : Float
    let int_type = Term::type_constant("Int");
    let float_type = Term::type_constant("Float");
    let one = Term::variable("one").has_type(int_type.clone());
    let let_binding = Term::let_binding("x", one, Term::variable("x").has_type(float_type));

    let mut global_types = HashMap::new();
    global_types.insert("one", int_type);
    global_types.insert("Int", Term::type_of_type());
    global_types.insert("Float", Term::type_of_type());
    let ctx = &mut Context::new(global_types);

    assert!(let_binding.link(ctx).is_err());
}
