use crate::{
	error::ParseError,
	expect,
	lexer::{Delim, Token, TokenKind},
};

use super::{instruction::Instruction, Parse, TokenStream};

#[derive(Debug, PartialEq, Eq, Clone)]
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
			if let TokenKind::CloseDelim(Delim::Bracket) = token {
				break;
			}

			instructions.push(Instruction::parse(tokens)?);
		}

		expect!(tokens, [CloseDelim(Delim::Bracket) => CloseDelim(Delim::Bracket)]);

		Ok(Self { instructions })
	}
}
