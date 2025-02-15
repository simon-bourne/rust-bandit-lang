use std::collections::HashMap;

use bandit_parser::{
    lex::{SrcToken, Token},
    parse::expr,
};
use bandit_types::{context::Context, inference, source::Expression, InferenceError, Pretty};
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
        global_items.insert(typ, Expression::type_constant(typ));
    }

    for (name, typ) in items {
        global_items.insert(name, Expression::constant(name, parse(typ)));
    }

    Context::new(global_items)
}

trait Test {
    fn infers(self, expected: &str);

    fn fails(self);
}

impl Test for &str {
    fn infers(self, expected: &str) {
        let expr = infer_types(self).unwrap();
        assert_eq!(expr.to_pretty_string(80), expected);
    }

    fn fails(self) {
        assert!(infer_types(self).is_err());
    }
}

fn infer_types(input: &str) -> Result<inference::Expression, InferenceError> {
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
    expr.infer_types()?;
    Ok(expr)
}

#[test]
fn one() {
    "one".infers("one : Int");
}

#[test]
fn simple_apply() {
    "abs one".infers("(abs : Int → Int) (one : Int) : Int");
}

#[test]
fn simple_lambda() {
    r"(\x ⇒ x : Int) : Int -> Int".infers(r"\x : Int ⇒ x : Int");
}

#[test]
fn self_referential_type() {
    r"\x ⇒ x : x".infers(r#"\x : Type = Type ⇒ Type"#);
}

#[test]
fn partial_add() {
    "add one".infers("(add : Int → Int → Int) (one : Int) : Int → Int");
}

#[test]
fn simple_id() {
    "id _ one".infers("((id : (∀a ⇒ a → a)) (Int : Type) : Int → Int) (one : Int) : Int");
}

#[test]
// TODO: This is supposed to fail
#[should_panic]
fn scope_escape() {
    "scoped _ id".fails()
}

#[test]
// TODO: Implement an occurs check
// Currently, this will happily infer an infinite type (and create `Rc` circular references). It
// will stack overflow when we try and convert the expression to a string.
#[ignore = "This tests the occurs check, which is not implemented yet"]
fn occurs_check() {
    r"\x ⇒ x x".infers("");
}

#[test]
fn multi_id() {
    "add (id Int one) (float_to_int (id Float pi))".infers(
        "((add : Int → Int → Int) (((id : (∀a ⇒ a → a)) (Int : Type) : Int → Int) (one : Int) : Int) : Int → Int) ((float_to_int : Float → Int) (((id : (∀a ⇒ a → a)) (Float : Type) : Float → Float) (pi : Float) : Float) : Int) : Int"
    );
}

#[test]
fn restrict_type() {
    "let id2 = id ⇒ (id2 : Type -> Int -> Int)"
        .infers("let id2 : Type → Int → Int = id ⇒ id : Type → Int → Int")
}

#[test]
fn polymorphic_let() {
    "let id2 : (∀a ⇒ a → a) = id ⇒ add (id2 Int one) (float_to_int (id2 Float pi))".infers(
        "let id2 : (∀a ⇒ a → a) = id ⇒ ((add : Int → Int → Int) (((id : (∀a ⇒ a → a)) (Int : Type) : Int → Int) (one : Int) : Int) : Int → Int) ((float_to_int : Float → Int) (((id : (∀a ⇒ a → a)) (Float : Type) : Float → Float) (pi : Float) : Float) : Int) : Int"
    );
}

#[test]
fn simple_polymorphic_lambda() {
    r"\x : (∀b ⇒ b) ⇒ x".infers(r"\x : (∀b ⇒ b) ⇒ x : (∀b ⇒ b)");
}

#[test]
fn simple_polymorphic_let() {
    "let x : (∀b ⇒ b) = polymorphic ⇒ x".infers(
        // TODO: We should have `x` on the RHS, not `polymorphic`.
        "let x : (∀b ⇒ b) = polymorphic ⇒ polymorphic : (∀b ⇒ b)",
    );
}

#[test]
fn polymorphic_lambda() {
    r"\id2 : (∀a ⇒ a → a) ⇒ add (id2 Int one) (float_to_int (id2 Float pi))".infers(
        r"\id2 : (∀a ⇒ a → a) ⇒ ((add : Int → Int → Int) (((id2 : (∀a ⇒ a → a)) (Int : Type) : Int → Int) (one : Int) : Int) : Int → Int) ((float_to_int : Float → Int) (((id2 : (∀a ⇒ a → a)) (Float : Type) : Float → Float) (pi : Float) : Float) : Int) : Int",
    );
}
