use std::collections::HashMap;

use bandit_parser::{
    lex::{SrcToken, Token},
    parse::term,
};
use bandit_types::{context::Context, inference, source::Term, InferenceError, Pretty};
use winnow::Parser;

fn parse(input: &str) -> Term<'_> {
    let tokens: Vec<SrcToken> = Token::layout(input).collect();
    term.parse(&tokens).unwrap()
}

fn context<'src>(
    types: impl IntoIterator<Item = &'src str>,
    items: impl IntoIterator<Item = (&'src str, &'src str)>,
) -> Context<'src> {
    let mut global_items = HashMap::new();

    for typ in types {
        global_items.insert(typ, Term::type_constant(typ));
    }

    for (name, typ) in items {
        global_items.insert(name, Term::constant(name, parse(typ)));
    }

    Context::new(global_items)
}

trait Test {
    fn infers(self, expected: &str);

    fn fails(self);
}

impl Test for &str {
    fn infers(self, expected: &str) {
        let term = infer_types(self).unwrap();
        assert_eq!(term.to_pretty_string(80), expected);
    }

    fn fails(self) {
        assert!(infer_types(self).is_err());
    }
}

fn infer_types(input: &str) -> Result<inference::Term, InferenceError> {
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

    let mut term = parse(input).link(ctx).unwrap();
    term.infer_types(0)?;
    Ok(term)
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
// TODO: This test should pass (i.e. inference should fail)
#[should_panic]
fn scope_escape() {
    "scoped _ id".fails()
}

#[test]
// TODO: Implement an occurs check
// Currently, this will happily infer an infinite type (and create `Rc` circular references). It
// will stack overflow when we try and convert the term to a string.
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
        .infers("let id2 : Type → Int → Int = id ⇒ id2 : Type → Int → Int")
    // TODO: This should be:
    // `.infers("let id2 : (∀a ⇒ a → a) = id ⇒ id2 : Type → Int → Int")`
}

#[test]
fn restrict_explicit_type() {
    "let id2 : (∀a ⇒ a → a) = id ⇒ (id2 : Type -> Int -> Int)"
        .infers("let id2 : Type → Int → Int = id ⇒ id2 : Type → Int → Int")
    // TODO: This should be:
    // `.infers("let id2 : (∀a ⇒ a → a) = id ⇒ id2 : Type → Int → Int")`
    // It happens because both occurances of `id2` reference the same term in
    // memory, so the 2 types get unified.
}

#[test]
fn polymorphic_let() {
    "let id2 : (∀a ⇒ a → a) = id ⇒ add (id2 Int one) (float_to_int (id2 Float pi))".infers(
        "let id2 : (∀a ⇒ a → a) = id ⇒ ((add : Int → Int → Int) (((id2 : (∀a ⇒ a → a)) (Int : Type) : Int → Int) (one : Int) : Int) : Int → Int) ((float_to_int : Float → Int) (((id2 : (∀a ⇒ a → a)) (Float : Type) : Float → Float) (pi : Float) : Float) : Int) : Int"
    );
}

#[test]
fn simple_polymorphic_lambda() {
    r"\x : (∀b ⇒ b) ⇒ x".infers(r"\x : (∀b ⇒ b) ⇒ x : (∀b ⇒ b)");
}

#[test]
fn simple_polymorphic_let() {
    "let x : (∀b ⇒ b) = polymorphic ⇒ x".infers("let x : (∀b ⇒ b) = polymorphic ⇒ x : (∀b ⇒ b)");
}

#[test]
fn polymorphic_lambda() {
    r"\id2 : (∀a ⇒ a → a) ⇒ add (id2 Int one) (float_to_int (id2 Float pi))".infers(
        r"\id2 : (∀a ⇒ a → a) ⇒ ((add : Int → Int → Int) (((id2 : (∀a ⇒ a → a)) (Int : Type) : Int → Int) (one : Int) : Int) : Int → Int) ((float_to_int : Float → Int) (((id2 : (∀a ⇒ a → a)) (Float : Type) : Float → Float) (pi : Float) : Float) : Int) : Int",
    );
}
