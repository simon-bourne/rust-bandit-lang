use std::collections::HashMap;

use bandit_term::{
    Evaluation,
    ast::Term,
    context::{ContextOwner, Value},
};

#[test]
fn let_error() {
    // let x : Int = 1 in x : Float
    let cant_unify = || {
        let x = || Term::variable("x");
        let int_type = || Term::variable("Int");
        let float_type = Term::variable("Float");
        let one = Term::variable("one").has_type(int_type());
        let let_binding =
            Term::let_binding(x(), one, x().has_type(float_type), Evaluation::Dynamic);

        let mut global_types = HashMap::new();
        global_types.insert("one", Value::new(int_type()));
        global_types.insert("Int", Value::new(Term::type_of_type()));
        global_types.insert("Float", Value::new(Term::type_of_type()));
        let ctx_owner = ContextOwner::new([], global_types);
        let mut ctx = ctx_owner.handle();
        let_binding.desugar(&ctx)?;

        ctx.infer_types()
    };

    cant_unify().unwrap_err();
}
