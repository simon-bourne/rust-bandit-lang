use std::{cell::RefCell, fmt, rc::Rc, result};

pub mod context;
pub mod inference;
mod pretty;
pub mod source;

pub use pretty::Pretty;

type SharedMut<T> = Rc<RefCell<T>>;

pub type Result<T> = result::Result<T, InferenceError>;

#[derive(Debug)]
pub struct InferenceError;

// TODO: Rename
pub trait ExpressionReference<'src>: Pretty {
    type Variable: Pretty;
}

enum Expression<'src, Expr: ExpressionReference<'src>> {
    TypeOfType,
    Constant {
        name: &'src str,
        typ: Expr,
    },
    Apply {
        function: Expr,
        argument: Expr,
        typ: Expr,
    },
    Let(VariableBinding<'src, Expr>),
    FunctionType(VariableBinding<'src, Expr>),
    Lambda(VariableBinding<'src, Expr>),
    Variable(Expr::Variable),
}

struct VariableBinding<'src, Expr: ExpressionReference<'src>> {
    name: &'src str,
    variable_value: Expr,
    in_expression: Expr,
}

#[derive(Copy, Clone, Eq, PartialEq)]
struct DeBruijnIndex(usize);

#[derive(Copy, Clone, Eq, PartialEq)]
enum VariableScope {
    Local(DeBruijnIndex),
    Global,
}

// TODO: Make private. need to make `SourceExpression` and
// `NamesResolvedExpression` newtypes
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Variable<'src> {
    name: &'src str,
    scope: VariableScope,
}

impl fmt::Debug for Variable<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.to_pretty_string(80))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{context::Context, source::SourceExpression as Expr, Pretty};

    #[test]
    fn infer_kinds() {
        // C : (m a)
        let m = Expr::variable("m");
        let a = Expr::variable("a");
        let ctx = &mut Context::new(HashMap::new());
        let mut constructor_type = Expr::lambda(
            "m",
            Expr::unknown_type(),
            Expr::lambda("a", Expr::unknown_type(), Expr::apply(m, a)),
        )
        .link(ctx)
        .unwrap();

        constructor_type.infer_types().unwrap();
        assert_eq!(
            constructor_type.to_pretty_string(80),
            r"\m : _ → _ ⇒ \a ⇒ (m : _ → _) a : _"
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
        global_types.insert("one", int_type.resolve_names().unwrap());
        global_types.insert("Int", Expr::type_of_type().resolve_names().unwrap());
        global_types.insert("Float", Expr::type_of_type().resolve_names().unwrap());
        let ctx = &mut Context::new(global_types);

        assert!(let_binding.link(ctx).is_err());
    }
}
