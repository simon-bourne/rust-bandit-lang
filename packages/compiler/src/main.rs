use bandit_compiler::{lex::RichError, lexer};
use chumsky::Parser;

fn main() {
    // TODO: Tests
    // TODO: Literals

    let lines = lexer::<RichError>().padded().parse(
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
    expr if 'lt ++++ 
"#,
    );
    println!("{:#?}", lines.output());
    println!("{:?}", lines.errors().collect::<Vec<_>>());

    if let Some(lines) = lines.output() {
        println!("{lines}");
    }
}
