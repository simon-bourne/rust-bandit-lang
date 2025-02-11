use std::collections::HashMap;

use bandit_parser::{
    lex::{SrcToken, Token},
    parse::expr,
};
use bandit_types::{context::Context, type_annotated::named_locals::Expression, Pretty};
use winnow::Parser;

fn parse(input: &str) -> Expression<'_> {
    let tokens: Vec<SrcToken> = Token::layout(input).collect();
    expr.parse(&tokens).unwrap()
}

fn context<'src>(
    types: impl IntoIterator<Item = &'src str>,
    items: impl IntoIterator<Item = (&'src str, &'src str)>,
) -> Context<'src> {
    let mut global_items = HashMap::new();

    for typ in types {
        global_items.insert(typ, Expression::type_constant(typ).resolve_names().unwrap());
    }

    for (name, typ) in items {
        global_items.insert(
            name,
            Expression::constant(name, parse(typ))
                .resolve_names()
                .unwrap(),
        );
    }

    Context::new(global_items)
}

fn expect_inferred(input: &str, expected: &str) {
    let ctx = &mut context(
        ["Bool", "Int", "Float"],
        [
            ("one", "Int"),
            ("pi", "Float"),
            ("true", "Bool"),
            ("abs", "Int → Int"),
            ("add", "Int → Int → Int"),
            ("id", "∀a ⇒ a → a"),
            ("float_to_int", "Float → Int"),
            ("polymorphic", "∀a ⇒ a"),
            ("scoped", "∀a ⇒ (∀s ⇒ s -> a) -> a"),
        ],
    );

    let mut expr = parse(input).link(ctx).unwrap();
    expr.infer_types().unwrap();
    assert_eq!(expr.to_pretty_string(80), expected);
}

#[test]
fn one() {
    expect_inferred("one", "one : Int");
}

#[test]
fn simple_apply() {
    expect_inferred("abs one", "(abs : Int → Int) (one : Int) : Int");
}

#[test]
fn simple_lambda() {
    expect_inferred(r"(\x ⇒ x : Int) : Int -> Int", r"\x : Int ⇒ x : Int");
}

#[test]
#[should_panic]
fn weird_lambda() {
    // TODO: This should return an `InferenceError` or infer the type of `x` to be
    // `Type`, rather than borrow error.
    expect_inferred(r"\x ⇒ x : x", r"");
}

#[test]
fn partial_add() {
    expect_inferred("add one", "(add : Int → Int → Int) (one : Int) : Int → Int");
}

#[test]
fn simple_id() {
    expect_inferred(
        "id _ one",
        "((id : (∀a ⇒ a → a)) (Int : Type) : Int → Int) (one : Int) : Int",
    );
}

// TODO: This should fail
#[test]
fn scope_escape() {
    expect_inferred(
        "scoped _ id",
        "((scoped : (∀a ⇒ (∀s ⇒ s → a) → a)) (a : Type) : (∀a ⇒ a → a) → a) (id : (∀a ⇒ a → a))",
    )
}

#[test]
fn multi_id() {
    expect_inferred(
        "add (id Int one) (float_to_int (id Float pi))",
        "((add : Int → Int → Int) (((id : (∀a ⇒ a → a)) (Int : Type) : Int → Int) (one : Int) : Int) : Int → Int) ((float_to_int : Float → Int) (((id : (∀a ⇒ a → a)) (Float : Type) : Float → Float) (pi : Float) : Float) : Int) : Int"
    );
}

// TODO: This should fail. The value of the first argument is inferred to be
// always `Int : Type`, so we'd just need to check all values are unknown.
#[test]
fn unsoundness() {
    expect_inferred(
        "let id2 = id ⇒ (id2 : Type -> Int -> Int)",
        "let id2 : Type → Int → Int = id ⇒ id : Type → Int → Int",
    )
}

#[test]
fn polymorphic_let() {
    expect_inferred(
        "let id2 : (∀a ⇒ a → a) = id ⇒ add (id2 Int one) (float_to_int (id2 Float pi))",
        "let id2 : (∀a ⇒ a → a) = id ⇒ ((add : Int → Int → Int) (((id : (∀a ⇒ a → a)) (Int : Type) : Int → Int) (one : Int) : Int) : Int → Int) ((float_to_int : Float → Int) (((id : (∀a ⇒ a → a)) (Float : Type) : Float → Float) (pi : Float) : Float) : Int) : Int"
    );
}

#[test]
fn simple_polymorphic_lambda() {
    expect_inferred(r"\x : (∀b ⇒ b) ⇒ x", r"\x : (∀b ⇒ b) ⇒ x : (∀b ⇒ b)");
}

#[test]
fn simple_polymorphic_let() {
    expect_inferred(
        "let x : (∀b ⇒ b) = polymorphic ⇒ x",
        // TODO: We should have `x` on the RHS, not `polymorphic`.
        "let x : (∀b ⇒ b) = polymorphic ⇒ polymorphic : (∀b ⇒ b)",
    );
}

#[test]
fn polymorphic_lambda() {
    expect_inferred(
        r"\id2 : (∀a ⇒ a → a) ⇒ add (id2 Int one) (float_to_int (id2 Float pi))",
        r"\id2 : (∀a ⇒ a → a) ⇒ ((add : Int → Int → Int) (((id2 : (∀a ⇒ a → a)) (Int : Type) : Int → Int) (one : Int) : Int) : Int → Int) ((float_to_int : Float → Int) (((id2 : (∀a ⇒ a → a)) (Float : Type) : Float → Float) (pi : Float) : Float) : Int) : Int",
    );
}
