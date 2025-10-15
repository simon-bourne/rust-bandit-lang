use anyhow::{Result, anyhow};
use bandit_parser::{
    lex::{SrcToken, Token},
    parse::definitions,
};
use bandit_types::{Pretty, context::Context, source::FunctionDefinition};
use winnow::Parser;

pub fn compile(source: &str) -> Result<()> {
    let tokens: Vec<SrcToken> = Token::layout(source).collect();
    let source = definitions()
        .parse(&tokens)
        .map_err(|_| anyhow!("A parse error occurred"))?;

    println!("Before type inference");
    print_definitions(&source);

    let mut ctx = Context::new(
        source
            .iter()
            .map(|FunctionDefinition { name, typ, .. }| (*name, typ.clone())),
    );

    // TODO: We need to transform `FunctionDefinition<source::Term>` to
    // `FunctionDefinition<linked::Term>`, which will have its types inferred
    for FunctionDefinition { typ, value, .. } in &source {
        if let Some(value) = value {
            value
                .clone()
                .has_type(typ.clone())
                .link(&mut ctx)
                .map_err(|_| anyhow!("A link error occurred"))?;
        }
    }

    ctx.constraints()
        .solve()
        .map_err(|_| anyhow!("A type inference error occurred"))?;

    println!("After type inference");
    print_definitions(&source);

    Ok(())
}

fn print_definitions(source: &[FunctionDefinition<'_>]) {
    for FunctionDefinition { name, typ, value } in source {
        print!("{name} : {}", typ.to_pretty_string(80));

        if let Some(value) = value {
            println!(" = {}", value.to_pretty_string(80));
        } else {
            println!();
        }
    }
}
