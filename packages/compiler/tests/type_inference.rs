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

fn test_with_ctx(input: &str, expected: &str) {
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
fn simple_apply() {
    test_with_ctx("abs one", "(abs : Int → Int) (one : Int) : Int");
}

#[test]
fn simple_lambda() {
    // TODO: Why isn't this `(\x : Int ⇒ x : Int) : Int → Int`
    test_with_ctx(r"\x => x : Int", r"\x : Int ⇒ x : Int");
}

#[test]
#[should_panic]
fn weird_lambda() {
    // TODO: This should return an `InferenceError` or infer the type of `x` to be
    // `Type`, rather than borrow error.
    test_with_ctx(r"\x => x : x", r"");
}

#[test]
fn partial_add() {
    test_with_ctx("add one", "(add : Int → Int → Int) (one : Int) : Int → Int");
}

#[test]
fn simple_id() {
    test_with_ctx(
        "id _ one",
        "((id : (∀a = Int ⇒ Int → Int)) (Int : Type) : Int → Int) (one : Int) : Int",
    );
}

#[test]
fn multi_id() {
    test_with_ctx(
        "add (id Int one) (float_to_int (id Float pi))",
        "((add : Int → Int → Int) (((id : (∀a = Int ⇒ Int → Int)) (Int : Type) : Int → Int) (one : Int) : Int) : Int → Int) ((float_to_int : Float → Int) (((id : (∀a = Float ⇒ Float → Float)) (Float : Type) : Float → Float) (pi : Float) : Float) : Int) : Int"
    );
}

// TODO: This should fail
#[test]
fn unsoundness() {
    test_with_ctx(
        "let id2 = id ⇒ (id2 : Type -> Int -> Int)",
        "let id2 : Type → Int → Int = id ⇒ id2 : Type → Int → Int = id",
    )
}

#[test]
// TODO: This shouldn't fail
#[should_panic]
fn polymorphic_let() {
    test_with_ctx(
        "let id2 : (∀a ⇒ a → a) = id ⇒ add (id2 Int one) (float_to_int (id2 Float pi))",
        "let id2 : (∀a ⇒ a → a) = id ⇒ ((add : Int → Int → Int) (((id2 : (∀a = Int ⇒ Int → Int) = id) (Int : Type) : Int → Int) (one : Int) : Int) : Int → Int) ((float_to_int : Float → Int) (((id2 : (∀a = Float ⇒ Float → Float) = id) (Float : Type) : Float → Float) (pi : Float) : Float) : Int) : Int"
    );
}

#[test]
fn polymorphic_lambda() {
    test_with_ctx(
        r"\id2 : (∀a ⇒ a → a) ⇒ add (id2 Int one) (float_to_int (id2 Float pi))",
        r"\id2 : (∀a ⇒ a → a) ⇒ ((add : Int → Int → Int) (((id2 : (∀a = Int ⇒ Int → Int)) (Int : Type) : Int → Int) (one : Int) : Int) : Int → Int) ((float_to_int : Float → Int) (((id2 : (∀a = Float ⇒ Float → Float)) (Float : Type) : Float → Float) (pi : Float) : Float) : Int) : Int",
    );
}
