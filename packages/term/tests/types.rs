use std::collections::HashMap;

use bandit_term::{
    Pretty,
    ast::Term,
    context::{ContextOwner, Value},
};

#[test]
fn infer_kinds() {
    // C : (m a)
    let m = || Term::variable("m");
    let a = || Term::variable("a");
    let ctx_owner = ContextOwner::new([], []);
    let ctx = ctx_owner.handle();
    let constructor_type = Term::lambda(m(), Term::lambda(a(), Term::apply(m(), a())))
        .desugar(&ctx)
        .unwrap();
    ctx.infer_types().unwrap();

    assert_eq!(
        constructor_type.to_pretty_string(80),
        r"\m : _ → _ ⇒ \a ⇒ (m : _ → _) a"
    );
}

#[test]
fn let_error() {
    // let x : Int = 1 in x : Float
    let x = || Term::variable("x");
    let int_type = || Term::variable("Int");
    let float_type = Term::variable("Float");
    let one = Term::variable("one").has_type(int_type());
    let let_binding = Term::let_binding(x(), one, x().has_type(float_type));

    let mut global_types = HashMap::new();
    global_types.insert("one", Value::new(int_type()));
    global_types.insert("Int", Value::new(Term::type_of_type()));
    global_types.insert("Float", Value::new(Term::type_of_type()));
    let ctx_owner = ContextOwner::new([], global_types);
    let ctx = ctx_owner.handle();
    let_binding.desugar(&ctx).unwrap();

    assert!(ctx.infer_types().is_err());
}
