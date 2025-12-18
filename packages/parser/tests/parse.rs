use bandit_parser::{lex::Token, parse::term};
use bandit_term::Pretty;
use winnow::Parser;

#[test]
fn pi() {
    parse("∀x ⇒ x", "∀x ⇒ x");
}

#[test]
fn lambda() {
    parse(r"(\x ⇒ x) Type", r"(\x ⇒ x) Type");
}

#[test]
fn type_annotation() {
    parse("x : Int", "x : Int");
}

fn parse(input: &str, expected: &str) {
    let tokens = Token::layout(input);
    let term = term.parse(&tokens).unwrap();
    assert_eq!(term.to_pretty_string(80), expected);
}
