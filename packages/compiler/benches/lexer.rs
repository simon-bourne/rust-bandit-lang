use bandit_compiler::lex::{SrcToken, Token};
use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};
use logos::Logos;

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
                assert!(token.is_ok());
                black_box(&token);
            }
        })
    });
    group.bench_function("logos-errors", |b| {
        b.iter(|| {
            consume_lexer(Token::tokens(&input));
        })
    });
    group.bench_function("layout", |b| {
        b.iter(|| {
            consume_lexer(Token::layout(&input));
        })
    });
}

fn consume_lexer<'a>(lexer: impl Iterator<Item = SrcToken<'a>>) {
    for token in lexer {
        assert!(token.0 != Token::Error);
        black_box(&token);
    }
}

criterion_group!(benches, basic);
criterion_main!(benches);
