use bandit_compiler::lexer;
use chumsky::Parser;
use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};

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

    group.bench_function("parse", |b| {
        let lexer = lexer().padded();

        b.iter(|| {
            lexer.parse(black_box(&input));
        })
    });
}

criterion_group!(benches, basic);
criterion_main!(benches);
