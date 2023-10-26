use bandit_compiler::lexer;
use chumsky::Parser;

fn main() {
    // TODO: Tests
    // TODO: Other tokens

    let lines = lexer().padded().parse(
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
    println!("{:#?}", lines.output());
    println!("{:?}", lines.errors().collect::<Vec<_>>());

    if let Some(lines) = lines.output() {
        println!("{lines}");
    }
}
