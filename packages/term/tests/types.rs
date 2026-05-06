use std::collections::HashMap;

use bandit_term::{
    ast::{Declaration, Source, Term},
    context::{ContextOwner, Value},
};

#[test]
fn let_error() {
    // let x : Int = 1 in x : Float
    let cant_unify = || {
        let src = Source::begin();
        let int_type = || Term::variable(src, "Int");
        let float_type = Term::variable(src, "Float");
        let one = Term::variable(src, "one").has_type(int_type());
        let let_binding = Term::let_binding(
            src,
            Declaration::new(src, "x", None),
            one,
            Term::variable(src, "x").has_type(float_type),
        );

        let mut global_types = HashMap::new();
        global_types.insert("one", Value::new(src, int_type()));
        global_types.insert("Int", Value::new(src, Term::type_of_type(src)));
        global_types.insert("Float", Value::new(src, Term::type_of_type(src)));
        let ctx_owner = ContextOwner::new([], global_types);
        let mut ctx = ctx_owner.handle();
        let_binding.desugar(&ctx)?;

        ctx.infer_types()
    };

    cant_unify().unwrap_err();
}
