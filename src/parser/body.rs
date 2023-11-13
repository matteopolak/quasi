use crate::{
	error::ParseError,
	lexer::{Delim, Token, TokenKind},
};

use super::{instruction::Instruction, Parse, TokenStream};

#[derive(Debug, PartialEq, Eq)]
pub struct Body {
	pub instructions: Vec<Instruction>,
}

impl Parse for Body {
	fn parse(tokens: &mut TokenStream) -> Result<Self, ParseError>
	where
		Self: Sized,
	{
		match tokens.peek().map(Token::kind) {
			Some(TokenKind::OpenDelim(Delim::Bracket)) => (),
			None => {
				return Err(ParseError::expected(
					vec![TokenKind::OpenDelim(Delim::Bracket)],
					None,
				));
			}
			_ => {
				let instruction = Instruction::parse(tokens)?;

				return Ok(Self {
					instructions: vec![instruction],
				});
			}
		}

		tokens.next();

		let mut instructions = Vec::new();

		while let Some(Token { kind: token, .. }) = tokens.peek() {
			match token {
				TokenKind::CloseDelim(Delim::Bracket) => {
					tokens.next();
					break;
				}
				_ => {
					let instruction = Instruction::parse(tokens)?;

					instructions.push(instruction);
				}
			}
		}

		Ok(Self { instructions })
	}
}
