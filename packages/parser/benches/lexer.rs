use bandit_parser::lex::Token;
use criterion::{Criterion, Throughput, black_box, criterion_group, criterion_main};
use logos::Logos;

pub fn basic(c: &mut Criterion) {
    let mut input = String::new();

    for _i in 0..4922 {
        input.push_str(
            r#"
term
term
do
    (term term)
        term
    term term (do
            term term
            term
            term)
    do
            term
    do
        term
termterm
term do term term do
    term
    term
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
    group.bench_function("layout", |b| {
        b.iter(|| {
            let lexer = Token::iter(&input);

            for token in lexer {
                assert!(!matches!(token.0, Token::Error(_)));
                black_box(&token);
            }
        })
    });
}

criterion_group!(benches, basic);
criterion_main!(benches);
