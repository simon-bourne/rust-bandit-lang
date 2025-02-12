use std::collections::HashMap;

use bandit_types::{context::Context, type_annotated::Expression as Expr, Pretty};

#[test]
fn infer_kinds() {
    // C : (m a)
    let m = Expr::variable("m");
    let a = Expr::variable("a");
    let ctx = &mut Context::new(HashMap::new());
    let mut constructor_type = Expr::lambda(
        "m",
        Expr::unknown_value(),
        Expr::lambda("a", Expr::unknown_value(), Expr::apply(m, a)),
    )
    .link(ctx)
    .unwrap();

    constructor_type.infer_types().unwrap();
    assert_eq!(
        constructor_type.to_pretty_string(80),
        r"\m : _ → _ ⇒ \a ⇒ (m : _ → _) a"
    );
}

#[test]
fn let_error() {
    // let x : Int = 1 in x : Float
    let int_type = Expr::type_constant("Int");
    let float_type = Expr::type_constant("Float");
    let one = Expr::variable("one").has_type(int_type.clone());
    let let_binding = Expr::let_binding("x", one, Expr::variable("x").has_type(float_type));

    let mut global_types = HashMap::new();
    global_types.insert("one", int_type);
    global_types.insert("Int", Expr::type_of_type());
    global_types.insert("Float", Expr::type_of_type());
    let ctx = &mut Context::new(global_types);

    assert!(let_binding.link(ctx).is_err());
}
