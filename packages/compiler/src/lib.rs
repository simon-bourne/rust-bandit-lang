use anyhow::{Result, anyhow};
use bandit_parser::{
    lex::{SrcToken, Token},
    parse::definitions,
};
use bandit_term::{Pretty, ast::Definition};
use winnow::Parser;

pub fn compile(source: &str) -> Result<()> {
    let tokens: Vec<SrcToken> = Token::layout(source).collect();
    // TODO: Ergonomic parse errors: <https://docs.rs/winnow/latest/winnow/_tutorial/chapter_7/index.html#error-adaptation-and-rendering>
    let constant = definitions()
        .parse(&tokens)
        .map_err(|_| anyhow!("A parse error occurred"))?;

    println!("Before type inference:");

    for constant in &constant {
        println!("{}", constant.to_pretty_string(80));
    }

    let ctx = Definition::context(constant);
    ctx.infer_types()?;

    println!();
    println!("After type inference:");

    for (name, value) in ctx.constants() {
        let value = value?.to_pretty_string(80);
        println!("{name} = {value}",)
    }

    Ok(())
}
