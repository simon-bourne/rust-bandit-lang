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

    for FunctionDefinition { name, typ, .. } in source {
        println!("{name} : {}", typ.to_pretty_string(80));
    }

    Ok(())
}
