use indoc::indoc;
use itertools::Itertools;

use super::SrcToken;
use crate::lex::{Grouping, Token};

#[test]
fn empty() {
    test("", "")
}

#[test]
fn only_whitespace() {
    test("    ", "")
}

#[test]
fn blank_lines() {
    test("\n\n", "")
}

#[test]
fn no_indent() {
    test(
        indoc!(
            r#"
                    x
                    y
                    z
                "#
        ),
        "x , y , z",
    )
}

#[test]
fn no_top_level() {
    test(
        r#"
                    x
                    y
                    z
                "#,
        "<< x , y , z >>",
    )
}

#[test]
fn indent_dedent() {
    test(
        indoc!(
            r#"
                    x
                    y
                    z
                        a
                        b
                        c
                    p
                    q
                    r
                "#
        ),
        "x , y , z << a , b , c >> p , q , r",
    );
}

#[test]
fn indent_without_dedent() {
    test(
        indoc!(
            r#"
                    x
                    y
                    z
                        a
                        b
                        c
                "#
        ),
        "x , y , z << a , b , c >>",
    );
}

#[test]
fn line_continuation() {
    test(
        indoc!(
            r#"
                    x
                    y
                    z
                        + a
                        + b
                        + c
                "#
        ),
        "x , y , z + a + b + c",
    );
}

#[test]
fn nested_blocks() {
    test(
        indoc!(
            r#"
                    x
                        y
                            z
                                a
                        b
                            c
                "#
        ),
        "x << y << z << a >> >> b << c >> >>",
    );
}

#[test]
fn mismatched_dedent() {
    test(
        indoc!(
            r#"
                    a
                            b
                        c
                "#
        ),
        "a << b <error> c >>",
    );
}

#[test]
fn mismatched_double_dedent() {
    test(
        indoc!(
            r#"
                    a
                        b
                                c
                                    d
                            e
                "#
        ),
        "a << b << c << d <error> e >> >>",
    );
}

#[test]
fn mismatched_double_dedent_at_end() {
    test(
        indoc!(
            r#"
                    a
                            b
                                c
                        d
                "#
        ),
        "a << b << c <error> d >>",
    );
}

fn test(src: &str, expected: &str) {
    print!("Source:\n\n{src}\n");
    let result = unlex(Token::layout(src), src);

    assert_eq!(expected, result);
}

fn unlex<'a>(layout: impl Iterator<Item = SrcToken<'a>>, src: &str) -> String {
    let layout: Vec<_> = layout.collect();
    println!("Tokens: {layout:?}");
    layout
        .into_iter()
        .map(|(token, span)| match token {
            Token::Open(Grouping::Block) => "<<",
            Token::Close(Grouping::Block) => ">>",
            Token::LineEnd => ",",
            Token::Error(_) => "<error>",
            _ => &src[span],
        })
        .join(" ")
}
