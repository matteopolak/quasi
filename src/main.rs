mod cli;
mod error;
mod executor;
mod lexer;
mod parser;
mod span;

use std::{fs, io};

use clap::Parser as _;
pub use error::Error;

fn main() -> Result<(), Error> {
	let args = cli::Args::parse();
	let input = fs::read(args.path).unwrap();

	let tokens = lexer::tokenize(&input).collect::<Result<_, _>>().unwrap();
	let stream = parser::instructionify(tokens);
	let mut executor = stream.collect::<Result<executor::Executor, _>>()?;

	executor.execute(&mut io::stdout())
}
