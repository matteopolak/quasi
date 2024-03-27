#![warn(clippy::pedantic)]
#![allow(clippy::module_name_repetitions)]
#![feature(associated_type_defaults)]

mod cli;
mod error;
mod executor;
mod lexer;
mod parser;
mod span;

use std::{fs, io};

use clap::Parser as _;
pub use error::Error;

fn main() {
	let args = cli::Args::parse();
	let result = fs::read(args.path)
		.map_err(|e| Error::Io(e).format(&[]))
		.and_then(|input| wrap(&input).map_err(|e| e.format(&input)));

	if let Err(e) = result {
		eprintln!("{e}");
	}
}

fn wrap(input: &[u8]) -> Result<(), Error> {
	let tokens = lexer::tokenize(input).collect::<Result<_, _>>()?;
	let stream = parser::instructionify(tokens);
	let mut executor = stream.collect::<Result<executor::Executor, _>>()?;

	executor.execute(&mut io::stdout())?;

	Ok(())
}
