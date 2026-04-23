use anyhow::{Result, anyhow};
use bandit_parser::parse;
use bandit_term::{Pretty, ast::Definition};

pub fn compile(source: &str) -> Result<()> {
    // TODO: Ergonomic parse errors: <https://docs.rs/winnow/latest/winnow/_tutorial/chapter_7/index.html#error-adaptation-and-rendering>
    let constants = parse::definitions(source).map_err(|_| anyhow!("A parse error occurred"))?;

    println!("Before type inference:");

    for constant in &constants {
        println!("{}", constant.to_pretty_string(80));
    }

    let ctx_owner = Definition::context(constants);
    let mut ctx = ctx_owner.handle();
    ctx.infer_types()?;

    println!();
    println!("After type inference:");

    for (name, value) in ctx.constants() {
        let value = value?.to_pretty_string(80);
        println!("{name} = {value}",)
    }

    Ok(())
}
