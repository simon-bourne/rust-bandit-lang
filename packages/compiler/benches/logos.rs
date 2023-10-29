use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};
use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
enum Token {
    // Block start tokens
    #[token("do")]
    Do,
    #[token("else")]
    Else,
    #[token("match")]
    Match,
    #[token("loop")]
    Loop,
    #[token("then")]
    Then,
    #[token("record")]
    Record,
    #[token("where")]
    Where,
    #[token("with")]
    With,

    // Delimiters
    #[token("(")]
    OpenParenthesis,
    #[token(")")]
    CloseParenthesis,
    #[token("[")]
    OpenBracket,
    #[token("]")]
    CloseBracket,
    #[token("{")]
    OpenBrace,
    #[token("}")]
    CloseBrace,

    // Keywords
    #[token("if")]
    If,
    #[token("return")]
    Return,
    #[token("while")]
    While,
    #[token("alias")]
    Alias,
    #[token("forall")]
    Forall,
    #[token("infer")]
    Infer,
    #[token("module")]
    Module,
    #[token("let")]
    Let,
    #[token("Self")]
    SelfType,
    #[token("trait")]
    Trait,
    #[token("type")]
    Type,
    #[token("use")]
    Use,

    // Other identifiers
    #[token("\\")]
    Lambda,

    #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*")]
    Identifier,
    #[regex(r"'[_a-zA-Z][_a-zA-Z0-9]*")]
    Lifetime,
    #[regex(r"\$%\&\*\+\./<=>@\^\-\~\|")]
    Operator,

    // Whitespace
    #[regex(r"[\n\f][ \t]*")]
    Indentation,
    #[regex(r"[ \t]+")]
    Whitespace,
}

pub fn basic(c: &mut Criterion) {
    let mut input = String::new();

    for _i in 0..4922 {
        input.push_str(
            r#"
expr
expr
do
    (expr expr)
        expr
    expr expr (do
            expr expr
            expr
        expr)
    do
            expr
        do
            expr
exprexpr
expr do expr expr do
    expr
    expr
"#,
        );
    }

    let mut group = c.benchmark_group("valid");
    group.throughput(Throughput::Bytes(input.len() as u64));

    group.bench_function("logos", |b| {
        b.iter(|| {
            let lexer = Token::lexer(&input);

            for token in lexer {
                black_box(&token.unwrap());
            }
        })
    });
}

criterion_group!(benches, basic);
criterion_main!(benches);
