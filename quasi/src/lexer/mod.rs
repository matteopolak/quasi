pub mod token;
pub use token::*;

mod util;

use crate::Error;

#[macro_export]
macro_rules! expect {
	($tokens:ident, [$($kind:pat => $value:expr),*]) => {
		{
			use $crate::lexer::token::TokenKind::*;

			match $tokens.next() {
				#[allow(clippy::unnested_or_patterns, unused_parens)]
				Some(t @ $crate::lexer::token::Token { kind: ($($kind)|*), .. }) => t,
				other => return Err(ParseError::expected(vec![$($value),*], other.map(|o| o.kind))),
			}
		}
	};
}

pub trait Decode {
	type Error;

	fn decode(input: &[u8]) -> Result<(Option<Self>, &[u8]), Self::Error>
	where
		Self: Sized;
}

pub struct Tokenize<'i> {
	input: &'i [u8],
	offset: usize,
}

impl<'i> Tokenize<'i> {
	pub fn new(input: &'i [u8]) -> Self {
		Self { input, offset: 0 }
	}

	pub fn skip_comments(&mut self) {
		while let Some(b'#') = self.input.first() {
			self.input = &self.input[1..];

			// Skip while there are tokens and the token is not a newline
			while let Some(t) = self.input.first() {
				self.input = &self.input[1..];

				if t == &b'\n' {
					break;
				}
			}
		}
	}
}

impl Iterator for Tokenize<'_> {
	type Item = Result<Token, Error>;

	fn next(&mut self) -> Option<Self::Item> {
		self.skip_comments();

		let (mut token, r) = match Token::decode(self.input) {
			Ok((token, r)) => (token, r),
			Err(e) => {
				return Some(Err(e.offset(self.offset)));
			}
		};

		if let Some(t) = token {
			token = Some(t.offset(self.offset));
		}

		self.offset += self.input.len() - r.len();
		self.input = r;

		token.map(Ok)
	}
}

pub fn tokenize(input: &[u8]) -> Tokenize {
	Tokenize::new(input)
}

#[cfg(test)]
mod test {
	use super::*;

	fn parse(input: &[u8]) -> Vec<TokenKind> {
		let tokens = tokenize(input)
			.map(|t| t.map(|t| t.kind))
			.collect::<Result<Vec<_>, _>>();

		assert!(tokens.is_ok(), "{tokens:?}");

		tokens.unwrap()
	}

	#[test]
	fn tokenize_expr() {
		let input = b"1 + 2 * 3";
		let tokens = parse(input);

		assert_eq!(
			tokens,
			vec![
				TokenKind::Literal(Lit::Number(1.)),
				TokenKind::Whitespace,
				TokenKind::Op(Op::Add),
				TokenKind::Whitespace,
				TokenKind::Literal(Lit::Number(2.)),
				TokenKind::Whitespace,
				TokenKind::Op(Op::Mul),
				TokenKind::Whitespace,
				TokenKind::Literal(Lit::Number(3.)),
			]
		);
	}

	#[test]
	fn tokenize_comments() {
		let input = b"1 + 2 # This is a comment\n* 3";
		let tokens = parse(input);

		assert_eq!(
			tokens,
			vec![
				TokenKind::Literal(Lit::Number(1.)),
				TokenKind::Whitespace,
				TokenKind::Op(Op::Add),
				TokenKind::Whitespace,
				TokenKind::Literal(Lit::Number(2.)),
				TokenKind::Whitespace,
				TokenKind::Op(Op::Mul),
				TokenKind::Whitespace,
				TokenKind::Literal(Lit::Number(3.)),
			]
		);
	}

	#[test]
	fn tokenize_while() {
		let input = b"while 1 < 2 { 3 }";
		let tokens = parse(input);

		assert_eq!(
			tokens,
			vec![
				TokenKind::Keyword(Symbol::While),
				TokenKind::Whitespace,
				TokenKind::Literal(Lit::Number(1.)),
				TokenKind::Whitespace,
				TokenKind::Cmp(Cmp::Lt),
				TokenKind::Whitespace,
				TokenKind::Literal(Lit::Number(2.)),
				TokenKind::Whitespace,
				TokenKind::OpenDelim(Delim::Brace),
				TokenKind::Whitespace,
				TokenKind::Literal(Lit::Number(3.)),
				TokenKind::Whitespace,
				TokenKind::CloseDelim(Delim::Brace),
			]
		);
	}
}
