use bandit_parser::{
    lex::{SrcToken, Token},
    parse::term,
};
use bandit_types::Pretty;
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
    let tokens: Vec<SrcToken> = Token::layout(input).collect();
    let term = term.parse(&tokens).unwrap();
    assert_eq!(term.to_pretty_string(80), expected);
}
