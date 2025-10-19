use anyhow::{Result, anyhow};
use bandit_parser::{
    lex::{SrcToken, Token},
    parse::definitions,
};
use bandit_types::{Pretty, source::FunctionDefinition};
use winnow::Parser;

pub fn compile(source: &str) -> Result<()> {
    let tokens: Vec<SrcToken> = Token::layout(source).collect();
    let source = definitions()
        .parse(&tokens)
        .map_err(|_| anyhow!("A parse error occurred"))?;

    println!("Before type inference:");

    for definition in &source {
        println!("{}", definition.to_pretty_string(80));
    }

    let ctx = FunctionDefinition::context(source);
    ctx.infer_types()
        .map_err(|_| anyhow!("A type inference error occurred"))?;

    println!();
    println!("After type inference:");

    for (name, value) in ctx.constants() {
        // TODO: We need a `linked::FunctionDefinition : Pretty`
        println!(
            "{name} = {}",
            value
                .map_err(|_| anyhow!("A type linking error occurred"))?
                .to_pretty_string(80)
        )
    }

    Ok(())
}
