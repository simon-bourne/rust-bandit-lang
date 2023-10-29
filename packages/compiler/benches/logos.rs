use bandit_compiler::lex::{BlockType, Delimiter, Keyword};
use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};
use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t]+")]
enum Token {
    #[token("do", |_| BlockType::Do)]
    #[token("else", |_| BlockType::Else)]
    #[token("match", |_| BlockType::Match)]
    #[token("loop", |_| BlockType::Loop)]
    #[token("then", |_| BlockType::Then)]
    #[token("record", |_| BlockType::Record)]
    #[token("where", |_| BlockType::Where)]
    #[token("with", |_| BlockType::With)]
    Block(BlockType),

    #[token("(", |_| Delimiter::Parentheses)]
    #[token("[", |_| Delimiter::Brackets)]
    #[token("{", |_| Delimiter::Braces)]
    Open(Delimiter),

    #[token(")", |_| Delimiter::Parentheses)]
    #[token("]", |_| Delimiter::Brackets)]
    #[token("}", |_| Delimiter::Braces)]
    Close(Delimiter),

    #[token("if", |_| Keyword::If)]
    #[token("return", |_| Keyword::Return)]
    #[token("while", |_| Keyword::While)]
    #[token("alias", |_| Keyword::Alias)]
    #[token("forall", |_| Keyword::Forall)]
    #[token("infer", |_| Keyword::Infer)]
    #[token("module", |_| Keyword::Module)]
    #[token("let", |_| Keyword::Let)]
    #[token("Self", |_| Keyword::SelfType)]
    #[token("trait", |_| Keyword::Trait)]
    #[token("type", |_| Keyword::Type)]
    #[token("use", |_| Keyword::Use)]
    Keyword(Keyword),

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
