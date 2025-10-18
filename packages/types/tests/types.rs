use std::collections::HashMap;

use bandit_types::{Pretty, context::Context, source::Term};

#[test]
fn infer_kinds() {
    // C : (m a)
    let m = Term::variable("m");
    let a = Term::variable("a");
    let ctx = &mut Context::new([]);
    let constructor_type = Term::lambda(m.clone(), Term::lambda(a.clone(), Term::apply(m, a)))
        .link(ctx)
        .unwrap();
    ctx.constraints().solve().unwrap();

    assert_eq!(
        constructor_type.to_pretty_string(80),
        r"\m : _ → _ ⇒ \a ⇒ (m : _ → _) a"
    );
}

#[test]
fn let_error() {
    // let x : Int = 1 in x : Float
    let x = Term::variable("x");
    let int_type = Term::variable("Int");
    let float_type = Term::variable("Float");
    let one = Term::variable("one").has_type(int_type.clone());
    let let_binding = Term::let_binding(x.clone(), one, x.has_type(float_type));

    let mut global_types = HashMap::new();
    global_types.insert("one", int_type);
    global_types.insert("Int", Term::typ());
    global_types.insert("Float", Term::typ());
    let ctx = &mut Context::new(global_types);
    let_binding.link(ctx).unwrap();

    assert!(ctx.constraints().solve().is_err());
}
