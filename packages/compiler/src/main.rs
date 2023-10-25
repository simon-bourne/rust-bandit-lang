use chumsky::{
    primitive::just,
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

fn parser<'a>() -> impl Parser<'a, &'a str, Vec<Statement<'a>>> {
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
        let token_separator = inline_whitespace().then(continue_line);
        let token = just("expr").map(TreeToken::Token);
        let open_block = just("do");
        let layout_block = open_block
            .then(newline())
            .ignore_then(block)
            .map(TreeToken::Do);
        let statement = token
            .or(layout_block)
            .separated_by(token_separator)
            .collect::<Vec<_>>()
            .map(Statement);
        let statement_separator = newline().then(blank_lines).then(indent);

        whitespace()
            .count()
            .ignore_with_ctx(statement.separated_by(statement_separator).collect())
    });

    block.with_ctx(0)
}

fn main() {
    // TODO: Test performance
    // TODO: Pretty printer
    // TODO: Handle inline `do`
    // TODO: Brackets

    let statements = parser().padded().parse(
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
    println!("{:#?}", statements.output());
    println!("{:?}", statements.errors().collect::<Vec<_>>());
}
