use std::path;

use clap::Parser;

/// Interpreter for the Quasi programming language
#[derive(Parser)]
#[command(author, version, about)]
pub struct Args {
	/// Path to the script to execute
	pub path: path::PathBuf,
}
