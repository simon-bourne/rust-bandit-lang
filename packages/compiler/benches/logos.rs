use bandit_compiler::lex::{BlockType, Delimiter, Keyword};
use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};
use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
#[logos(subpattern ident = r"(\p{XID_Start}|_)\p{XID_Continue}*")]
enum Token {
    #[token("do", |_| BlockType::Do)]
    #[token("else", |_| BlockType::Else)]
    #[token("loop", |_| BlockType::Loop)]
    #[token("match", |_| BlockType::Match)]
    #[token("record", |_| BlockType::Record)]
    #[token("then", |_| BlockType::Then)]
    #[token("where", |_| BlockType::Where)]
    Block(BlockType),

    #[token("(", |_| Delimiter::Parentheses)]
    #[token("[", |_| Delimiter::Brackets)]
    #[token("{", |_| Delimiter::Braces)]
    Open(Delimiter),

    #[token(")", |_| Delimiter::Parentheses)]
    #[token("]", |_| Delimiter::Brackets)]
    #[token("}", |_| Delimiter::Braces)]
    Close(Delimiter),

    #[token("alias", |_| Keyword::Alias)]
    #[token("forall", |_| Keyword::Forall)]
    #[token("if", |_| Keyword::If)]
    #[token("infer", |_| Keyword::Infer)]
    #[token("let", |_| Keyword::Let)]
    #[token("module", |_| Keyword::Module)]
    #[token("provide", |_| Keyword::Provide)]
    #[token("return", |_| Keyword::Return)]
    #[token("Self", |_| Keyword::SelfType)]
    #[token("trait", |_| Keyword::Trait)]
    #[token("type", |_| Keyword::Type)]
    #[token("use", |_| Keyword::Use)]
    #[token("while", |_| Keyword::While)]
    #[token("with", |_| Keyword::With)]
    Keyword(Keyword),

    #[token("\\")]
    Lambda,

    #[regex(r"(?&ident)")]
    Identifier,
    #[regex(r"'(?&ident)")]
    Lifetime,
    #[regex(r"\$%\&\*\+\./<=>@\^\-\~\|")]
    Operator,

    #[regex(r"[ \t\r\n\f]+")]
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
