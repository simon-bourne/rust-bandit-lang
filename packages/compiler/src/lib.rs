use anyhow::{Result, anyhow};
use bandit_parser::{
    lex::{SrcToken, Token},
    parse::definitions,
};
use bandit_types::{Pretty, source::Constant};
use winnow::Parser;

pub fn compile(source: &str) -> Result<()> {
    let tokens: Vec<SrcToken> = Token::layout(source).collect();
    let constant = definitions()
        .parse(&tokens)
        .map_err(|_| anyhow!("A parse error occurred"))?;

    println!("Before type inference:");

    for constant in &constant {
        println!("{}", constant.to_pretty_string(80));
    }

    let ctx = Constant::context(constant);
    ctx.infer_types()
        .map_err(|_| anyhow!("A type inference error occurred"))?;

    println!();
    println!("After type inference:");

    for (name, value) in ctx.constants() {
        // TODO: We need a `linked::Constant : Pretty`
        println!(
            "{name} = {}",
            value
                .map_err(|_| anyhow!("A type linking error occurred"))?
                .to_pretty_string(80)
        )
    }

    Ok(())
}
