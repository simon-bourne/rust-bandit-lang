use chumsky::{
    primitive::{choice, just},
    recursive::recursive,
    text::{inline_whitespace, newline, whitespace},
    ConfigIterParser, IterParser, Parser,
};

#[derive(Clone, Debug)]
struct Line<'src>(Vec<TreeToken<'src>>);

#[derive(Clone, Debug)]
enum TreeToken<'src> {
    Token(&'src str),
    Do(Vec<Line<'src>>),
}

fn lexer<'a>() -> impl Parser<'a, &'a str, Vec<Line<'a>>> {
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
                        .map(Line),
                )
                .map(|(_do, line)| TreeToken::Do(vec![line]))
        });
        let line = token
            .or(inline_block)
            .or(layout_block)
            .separated_by(token_separator)
            .collect()
            .map(Line);
        let line_separator = newline().then(blank_lines).then(indent);

        whitespace()
            .count()
            .ignore_with_ctx(line.separated_by(line_separator).collect())
    });

    block.with_ctx(0)
}

fn main() {
    // TODO: Benchmarks
    // TODO: Tests
    // TODO: Pretty printer
    // TODO: Brackets

    let lines = lexer().padded().parse(
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
    println!("{:#?}", lines.output());
    println!("{:?}", lines.errors().collect::<Vec<_>>());
}
