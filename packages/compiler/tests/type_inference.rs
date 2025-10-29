use bandit_parser::{
    lex::{SrcToken, Token},
    parse::term,
};
use bandit_types::{
    InferenceError, Pretty,
    ast::{Context, Term},
    core,
};
use winnow::Parser;

fn parse(input: &str) -> Term<'_> {
    let tokens: Vec<SrcToken> = Token::layout(input).collect();
    term.parse(&tokens).unwrap()
}

fn context<'src>(
    types: impl IntoIterator<Item = &'src str>,
    items: impl IntoIterator<Item = (&'src str, &'src str)>,
) -> Context<'src> {
    let types = types.into_iter().map(|name| (name, Term::type_of_type()));
    let items = items
        .into_iter()
        .map(|(name, typ)| (name, Term::unknown().has_type(parse(typ))));
    Context::new(types.chain(items))
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
        if let Ok(term) = infer_types(self) {
            panic!("Expected error, got '{}'", term.to_pretty_string(80))
        }
    }
}

fn infer_types(input: &str) -> Result<core::Term<'_>, InferenceError> {
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
            ("scoped", "∀a ⇒ (∀s ⇒ s → a) → a"),
        ],
    );

    let term = parse(input).desugar(ctx)?;
    ctx.infer_types()?;
    Ok(term)
}

#[test]
fn one() {
    "one".infers("one : Int");
}

#[test]
fn evaluate_let() {
    "one : let x = Int ⇒ x".infers("one : Int");
}

#[test]
fn evaluate_apply() {
    r"one : (\x ⇒ x) Int".infers("one : Int");
}

#[test]
fn evaluate_recursively() {
    r"one : let x = (\y ⇒ y) Int ⇒ x".infers("one : Int");
}

#[test]
fn evaluate_recursively2() {
    r"one : let x = Int ⇒ (\y ⇒ y) x".infers("one : Int");
}

#[test]
fn simple_apply() {
    "abs one".infers("(abs : Int → Int) (one : Int) : Int");
}

#[test]
fn simple_lambda() {
    r"(\x ⇒ x : Int) : Int → Int".infers(r"\x : Int ⇒ x : Int");
}

#[test]
fn bad_type_annotation() {
    "let id2 = id ⇒ (id2 : Type → Int → Int)".fails()
}

#[test]
// TODO: Need to think about this one.
// We can't currently unify `x` with `Type` as `x` is a bound variable. `x` will evaluate to `Type`,
// so maybe we shouldn't try to unify it until it's evaluated.
#[should_panic]
fn self_referential_type() {
    // TODO: This creates an `Rc` cycle, as `x : x`.
    r"let x = Type ⇒ x : x".infers(r#"let x : Type = Type ⇒ Type"#);
}

#[test]
fn partial_add() {
    "add one".infers("(add : Int → Int → Int) (one : Int) : Int → Int");
}

#[test]
fn simple_id() {
    "id @ _ one".infers("((id : (∀a ⇒ a → a)) @ (Int : Type) : Int → Int) (one : Int) : Int");
}

// TODO: How will we infer the type of `a`? Should unification produce `∀a = Int
// ⇒ a → a`?
// TODO: Can we remove brackets around `(∀a ⇒ a → a)`?
#[test]
fn infer_implicit_argument() {
    "id one".infers("(id : (∀a ⇒ a → a)) (one : Int) : Int");
}

#[test]
fn infer_type_is_type() {
    "∀a ⇒ a".infers("∀a : Type ⇒ a : Type");
}

#[test]
fn infer_implicit_argument_isolation() {
    "add (id one) (id @ Int one)".infers("((add : Int → Int → Int) ((id : (∀a ⇒ a → a)) (one : Int) : Int) : Int → Int) (((id : (∀a ⇒ a → a)) @ (Int : Type) : Int → Int) (one : Int) : Int) : Int");
}

// TODO: Implement a scope escape check
#[test]
#[ignore = "This tests the scope escape check, which is not implemented yet"]
fn scope_escape() {
    "scoped @ _ id".fails()
}

#[test]
fn occurs_check() {
    r"\x ⇒ x x".fails();
}

#[test]
fn multi_id() {
    "add (id @ Int one) (float_to_int (id @ Float pi))".infers(
        "((add : Int → Int → Int) (((id : (∀a ⇒ a → a)) @ (Int : Type) : Int → Int) (one : Int) : Int) : Int → Int) ((float_to_int : Float → Int) (((id : (∀a ⇒ a → a)) @ (Float : Type) : Float → Float) (pi : Float) : Float) : Int) : Int"
    );
}

#[test]
fn polymorphic_let() {
    "let id2 : (∀a ⇒ a → a) = id ⇒ add (id2 @ Int one) (float_to_int (id2 @ Float pi))".infers(
        "let id2 : (∀a ⇒ a → a) = id ⇒ ((add : Int → Int → Int) (((id2 : (∀a ⇒ a → a)) @ (Int : Type) : Int → Int) (one : Int) : Int) : Int → Int) ((float_to_int : Float → Int) (((id2 : (∀a ⇒ a → a)) @ (Float : Type) : Float → Float) (pi : Float) : Float) : Int) : Int"
    );
}

#[test]
fn simple_polymorphic_lambda() {
    r"\x : (∀b ⇒ b) ⇒ x".infers(r"\x : (∀b ⇒ b) ⇒ x : (∀b ⇒ b)");
}

#[test]
fn simple_polymorphic_let() {
    "let x : (∀b ⇒ b) = polymorphic ⇒ x".infers("let x : (∀a ⇒ a) = polymorphic ⇒ x : (∀a ⇒ a)");
}

#[test]
fn polymorphic_lambda() {
    r"\id2 : (∀a ⇒ a → a) ⇒ add (id2 @ Int one) (float_to_int (id2 @ Float pi))".infers(
        r"\id2 : (∀a ⇒ a → a) ⇒ ((add : Int → Int → Int) (((id2 : (∀a ⇒ a → a)) @ (Int : Type) : Int → Int) (one : Int) : Int) : Int → Int) ((float_to_int : Float → Int) (((id2 : (∀a ⇒ a → a)) @ (Float : Type) : Float → Float) (pi : Float) : Float) : Int) : Int",
    );
}
