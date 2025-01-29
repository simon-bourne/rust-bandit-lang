use std::collections::HashMap;

use bandit_parser::{
    lex::{SrcToken, Token},
    parser::{expr, Expr},
};
use bandit_types::{context::Context, source::SourceExpression, Pretty};
use winnow::Parser;

fn parse(input: &str) -> SourceExpression<'_> {
    let tokens: Vec<SrcToken> = Token::layout(input).collect();
    expr.parse(&tokens).unwrap()
}

fn context<'src>(
    types: impl IntoIterator<Item = &'src str>,
    items: impl IntoIterator<Item = (&'src str, &'src str)>,
) -> Context<'src> {
    let mut global_items = HashMap::new();

    for typ in types {
        global_items.insert(typ, Expr::literal_type(typ).resolve_names().unwrap());
    }

    for (name, typ) in items {
        global_items.insert(
            name,
            Expr::unknown_with_type(parse(typ)).resolve_names().unwrap(),
        );
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
    test_with_ctx(
        "add one",
        "(add : {_ = _ : Int} → {_ = _ : Int} → Int) (one : Int) : {_ = _ : Int} → Int",
    );
}

#[test]
fn simple_id() {
    test_with_ctx(
        "id Int one",
        "((id : (∀{a = Int} ⇒ {_ = _ : Int} → Int)) Int : {_ = _ : Int} → Int) (one : Int) : Int",
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
