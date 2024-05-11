use std::{sync::mpsc, thread, time};

use quasi::{executor, lexer, parser, Error};
use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen]
pub fn execute(input: &[u8], timeout_ms: u64) -> Result<String, String> {
	let timeout = time::Duration::from_millis(timeout_ms);
	let (tx, rx) = mpsc::channel();

	thread::scope(move |s| {
		s.spawn(move || s.spawn(move || tx.send(wrap(input).map_err(|e| e.format(input)))));
	});

	rx.recv_timeout(timeout)
		.unwrap_or_else(|_| Err(format!("execution timed out after {}ms", timeout_ms)))
}

fn wrap(input: &[u8]) -> Result<String, Error> {
	let tokens = lexer::tokenize(input).collect::<Result<_, _>>()?;
	let stream = parser::instructionify(tokens);
	let mut executor = stream.collect::<Result<executor::Executor, _>>()?;

	let mut output = Vec::new();

	executor.execute(&mut output)?;

	Ok(String::from_utf8_lossy(&output).into_owned())
}
