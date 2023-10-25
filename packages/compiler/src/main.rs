use chumsky::{
    prelude::*,
    text::{inline_whitespace, newline},
};

#[derive(Clone, Debug)]
struct Stmt<'src>(Vec<Expr<'src>>);

#[derive(Clone, Debug)]
enum Expr<'src> {
    Expr(&'src str),
    Do(Vec<Stmt<'src>>),
}

fn parser<'a>() -> impl Parser<'a, &'a str, Vec<Stmt<'a>>> {
    let block = recursive(|block| {
        let indent = just(' ')
            .repeated()
            .configure(|cfg, parent_indent| cfg.exactly(*parent_indent));
        let blank_lines = inline_whitespace().then(newline()).repeated();
        let extra_indent = indent.then(inline_whitespace().at_least(1));
        let continue_line = newline()
            .then(blank_lines)
            .then(extra_indent)
            .repeated()
            .at_most(1);
        let word_separator = inline_whitespace().then(continue_line);
        let do_block = just("do")
            .then(text::newline())
            .ignore_then(block)
            .map(Expr::Do);
        let expr = just("expr").map(Expr::Expr);
        let stmt = expr
            .or(do_block)
            .separated_by(word_separator)
            .collect::<Vec<_>>()
            .map(Stmt);
        let stmt_separator = newline().then(blank_lines).then(indent);

        text::whitespace()
            .count()
            .ignore_with_ctx(stmt.separated_by(stmt_separator).collect())
    });

    block.with_ctx(0)
}

fn main() {
    // TODO: Test performance
    // TODO: Pretty printer
    // TODO: Handle inline `do`
    // TODO: Brackets
    // TODO: Handle do in middle of expression:
    // do
    //         expr
    //     expr

    let stmts = parser().padded().parse(
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
"#,
    );
    println!("{:#?}", stmts.output());
    println!("{:?}", stmts.errors().collect::<Vec<_>>());
}
