use chumsky::{
    prelude::*,
    text::{inline_whitespace, newline},
};

#[derive(Clone, Debug)]
enum Stmt<'src> {
    Expr(Vec<&'src str>),
    Do(Vec<Stmt<'src>>),
}

fn parser<'a>() -> impl Parser<'a, &'a str, Vec<Stmt<'a>>> {
    let block = recursive(|block| {
        let indent = just(' ')
            .repeated()
            .configure(|cfg, parent_indent| cfg.exactly(*parent_indent));
        // TODO: Ignore blank lines
        let word_separator = inline_whitespace().then_ignore(
            newline()
                .then_ignore(indent)
                .then_ignore(inline_whitespace().at_least(1))
                .repeated()
                .at_most(1),
        );
        let expr = just("expr")
            .separated_by(word_separator)
            .collect::<Vec<_>>();

        let expr_stmt = expr.then_ignore(text::newline()).map(Stmt::Expr);
        let open_layout_block = just("do")
            .then(text::newline())
            .ignore_then(block)
            .map(Stmt::Do);
        let stmt = expr_stmt.or(open_layout_block);

        text::whitespace()
            .count()
            .ignore_with_ctx(stmt.separated_by(indent).collect())
    });

    block.with_ctx(0)
}

fn main() {
    // TODO: Handle `do` at end of line
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
    do
        expr expr
        expr
exprexpr
"#,
    );
    println!("{:#?}", stmts.output());
    println!("{:?}", stmts.errors().collect::<Vec<_>>());
}
