use bandit_parser::parse;
use bandit_term::Pretty;

#[test]
fn pi() {
    parse("∀x ⇒ x");
}

#[test]
fn implicit_function() {
    parse("x ⇒ x");
}

#[test]
fn explicit_function() {
    parse("x → x");
}

#[test]
fn lambda() {
    parse(r"(\x = x) Type");
}

#[test]
fn type_annotation() {
    parse("x : Int");
}

#[test]
fn parse_error() {
    fail("+", "error: \n  |\n1 | +\n  | ^")
}

fn parse(input: &str) {
    let term = parse::term(input).unwrap();
    assert_eq!(term.to_pretty_string(80), input);
}

fn fail(input: &str, expected: &str) {
    let message = parse::term(input).unwrap_err().to_string();
    assert_eq!(message, expected);
}
