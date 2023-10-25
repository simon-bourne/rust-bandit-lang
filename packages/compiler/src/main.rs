use chumsky::{
    primitive::{choice, just},
    recursive::recursive,
    text::{inline_whitespace, newline, whitespace},
    ConfigIterParser, IterParser, Parser,
};

#[derive(Clone, Debug)]
struct Statement<'src>(Vec<TreeToken<'src>>);

#[derive(Clone, Debug)]
enum TreeToken<'src> {
    Token(&'src str),
    Do(Vec<Statement<'src>>),
}

fn lexer<'a>() -> impl Parser<'a, &'a str, Vec<Statement<'a>>> {
    let open_block = just("do");
    let token = just("expr").map(TreeToken::Token);

    let blank_lines = inline_whitespace().then(newline()).repeated();

    let block = recursive(|block| {
        let indent = just(' ')
            .repeated()
            .configure(|cfg, parent_indent| cfg.exactly(*parent_indent));
        let extra_indent = indent.then(inline_whitespace().at_least(1));
        let continue_line = newline()
            .then(blank_lines)
            .then(extra_indent)
            .repeated()
            .at_most(1);
        let token_separator = inline_whitespace().then(continue_line);
        let layout_block = open_block
            .then(newline())
            .ignore_then(block)
            .map(TreeToken::Do);
        let inline_block = recursive(|inline_block| {
            open_block
                .then_ignore(inline_whitespace())
                .then(
                    choice((token, layout_block.clone(), inline_block))
                        .separated_by(inline_whitespace())
                        .collect()
                        .map(Statement),
                )
                .map(|(_do, statement)| TreeToken::Do(vec![statement]))
        });
        let statement = token
            .or(inline_block)
            .or(layout_block)
            .separated_by(token_separator)
            .collect()
            .map(Statement);
        let statement_separator = newline().then(blank_lines).then(indent);

        whitespace()
            .count()
            .ignore_with_ctx(statement.separated_by(statement_separator).collect())
    });

    block.with_ctx(0)
}

fn main() {
    // TODO: Benchmarks
    // TODO: Tests
    // TODO: Pretty printer
    // TODO: Brackets

    let statements = lexer().padded().parse(
        r#"
expr
expr
do
    expr expr
     expr
    expr expr do
            expr expr
            expr
        expr
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
    println!("{:#?}", statements.output());
    println!("{:?}", statements.errors().collect::<Vec<_>>());
}
