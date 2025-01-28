use std::collections::HashMap;

use bandit_parser::{
    lex::{SrcToken, Token},
    parser::{expr, Expr},
};
use bandit_types::{context::Context, source::NamesResolvedExpression, Pretty};
use winnow::Parser;

fn parse(input: &str) -> NamesResolvedExpression<'_> {
    let tokens: Vec<SrcToken> = Token::layout(input).collect();
    expr.parse(&tokens).unwrap().resolve_names().unwrap()
}

fn context<'src>(
    types: impl IntoIterator<Item = &'src str>,
    items: impl IntoIterator<Item = (&'src str, &'src str)>,
) -> Context<'src> {
    let mut global_items = HashMap::new();
    let type_of_type = Expr::type_of_type().resolve_names().unwrap();

    for typ in types {
        global_items.insert(typ, type_of_type.clone());
    }

    for (name, typ) in items {
        global_items.insert(name, parse(typ));
    }

    Context::new(global_items)
}

fn test_with_ctx(input: &str, expected: &str) {
    let ctx = &mut context(
        ["Bool", "Int", "Float"],
        [
            ("one", "Int"),
            ("pi", "Float"),
            ("true", "Bool"),
            ("add", "Int → Int → Int"),
            ("id", "∀a ⇒ a → a"),
            ("int_to_float", "Int → Float"),
        ],
    );

    let mut expr = parse(input).link(ctx).unwrap();
    expr.infer_types().unwrap();
    assert_eq!(expr.to_pretty_string(80), expected);
}

#[test]
fn one() {
    test_with_ctx("one", "one : Int");
}

#[test]
fn partial_add() {
    test_with_ctx("add one", "(add : Int → Int → Int) (one : Int) : Int → Int");
}

#[test]
// TODO: No it shouldn't
#[should_panic]
fn simple_id() {
    test_with_ctx(
        "id Int one",
        "((id : ∀a ⇒ a → a) Int : Int → Int) one : Int",
    );
}

#[test]
// TODO: No it shouldn't
#[should_panic]
fn multi_id() {
    test_with_ctx(
        "add (id Int one) (int_to_float (id Float pi))",
        "((add : Int → Int → Int) ((id : ∀a => a -> a) Int (one : Int)) : Int → Int) (int_to_float (((id : ∀a => a -> a) Float (pi : Float)))",
    );
}
