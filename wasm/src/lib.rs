use quasi::{executor, lexer, parser, Error};
use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen]
pub fn execute(input: &[u8]) -> Result<String, String> {
	wrap(input).map_err(|e| e.format(input))
}

fn wrap(input: &[u8]) -> Result<String, Error> {
	let tokens = lexer::tokenize(input).collect::<Result<_, _>>()?;
	let stream = parser::instructionify(tokens);
	let mut executor = stream.collect::<Result<executor::Executor, _>>()?;

	let mut output = Vec::new();

	executor.execute(&mut output)?;

	Ok(String::from_utf8_lossy(&output).into_owned())
}
