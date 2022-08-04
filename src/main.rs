#![allow(unused)]

use anyhow::{Context, Result};
use clap::Parser;
mod parser;
mod compiler;
mod types;
use std::{
    fs::File,
    io::{BufWriter, Write},
};

#[derive(Parser)]
struct Cli {
    #[clap(parse(from_os_str))]
    path: std::path::PathBuf,
}

fn main() -> Result<()> {
    //let args = Cli::parse();
    //let mut path = args.path;
    //let asm = parser::parse(&path)?;
    //path.set_extension("asm");
    //let mut f = File::create(&path)
    //    .with_context(|| format!("Unable to create file `{}`", path.display()))?;
    //f.write_all(asm.as_bytes())
    //    .with_context(|| format!("Unable to write to file `{}`", path.display()))?;
    Ok(())
}
