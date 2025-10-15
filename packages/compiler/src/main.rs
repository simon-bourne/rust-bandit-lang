use std::{fs, path::PathBuf};

use anyhow::Result;
use bandit_compiler::compile;
use clap::Parser;

fn main() -> Result<()> {
    let args = Cli::parse();

    compile(&fs::read_to_string(args.source)?)
}

#[derive(Parser)]
struct Cli {
    source: PathBuf,
}
