pub mod token;
pub use token::*;

mod util;

use crate::Error;

pub trait Decode {
	type Error;

	fn decode(input: &[u8]) -> Result<(Option<Self>, &[u8]), Self::Error>
	where
		Self: Sized;
}

pub struct Tokenize<'i> {
	input: &'i [u8],
	offset: usize,
	whitespace: bool,
}

impl<'i> Tokenize<'i> {
	pub fn new(input: &'i [u8]) -> Self {
		Self {
			input,
			offset: 0,
			whitespace: false,
		}
	}

	pub fn skip_comments(&mut self) {
		while let Some(b'#') = self.input.first() {
			self.input = &self.input[1..];

			// Skip while there are tokens and the token is not a newline
			while let Some(t) = self.input.first() {
				if t == &b'\n' {
					break;
				}

				self.input = &self.input[1..];
			}
		}
	}
}

impl Iterator for Tokenize<'_> {
	type Item = Result<Token, Error>;

	fn next(&mut self) -> Option<Self::Item> {
		self.skip_comments();

		let (token, r) = match Token::decode(self.input) {
			Ok((token, r)) => (token, r),
			Err(e) => {
				return Some(Err(e.offset(self.offset)));
			}
		};

		self.offset += self.input.len() - r.len();
		self.input = r;

		// De-duplicate adjacent whitespace tokens
		let whitespace = token.as_ref().map(Token::is_whitespace).unwrap_or(false);

		if self.whitespace && whitespace {
			return self.next();
		}

		self.whitespace = whitespace;
		token.map(Ok)
	}
}

pub fn tokenize(input: &[u8]) -> Tokenize {
	Tokenize::new(input)
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn tokenize_expr() {
		let input = b"1 + 2 * 3";
		let tokens = tokenize(input).collect::<Result<Vec<_>, _>>().unwrap();

		assert_eq!(
			tokens,
			vec![
				Token::new(TokenKind::Literal(Lit::Number(1.)), 0..1),
				Token::WHITESPACE,
				Token::new(TokenKind::Op(Op::Add), 2..3),
				Token::WHITESPACE,
				Token::new(TokenKind::Literal(Lit::Number(2.)), 4..5),
				Token::WHITESPACE,
				Token::new(TokenKind::Op(Op::Mul), 6..7),
				Token::WHITESPACE,
				Token::new(TokenKind::Literal(Lit::Number(3.)), 8..9),
			]
		);
	}

	#[test]
	fn tokenize_comments() {
		let input = b"1 + 2 # This is a comment\n* 3";
		let tokens = tokenize(input).collect::<Result<Vec<_>, _>>().unwrap();

		assert_eq!(
			tokens,
			vec![
				Token::new(TokenKind::Literal(Lit::Number(1.)), 0..1),
				Token::WHITESPACE,
				Token::new(TokenKind::Op(Op::Add), 2..3),
				Token::WHITESPACE,
				Token::new(TokenKind::Literal(Lit::Number(2.)), 4..5),
				Token::new(TokenKind::Op(Op::Mul), 22..23),
				Token::WHITESPACE,
				Token::new(TokenKind::Literal(Lit::Number(3.)), 24..25),
			]
		);
	}

	#[test]
	fn tokenize_while() {
		let input = b"while 1 < 2 { 3 }";
		let tokens = tokenize(input).collect::<Result<Vec<_>, _>>().unwrap();

		assert_eq!(
			tokens,
			vec![
				Token::new(TokenKind::Keyword(Symbol::While), 0..5),
				Token::WHITESPACE,
				Token::new(TokenKind::Literal(Lit::Number(1.)), 6..7),
				Token::WHITESPACE,
				Token::new(TokenKind::Cmp(Cmp::Lt), 8..9),
				Token::WHITESPACE,
				Token::new(TokenKind::Literal(Lit::Number(2.)), 10..11),
				Token::WHITESPACE,
				Token::new(TokenKind::OpenDelim(Delim::Brace), 12..13),
				Token::WHITESPACE,
				Token::new(TokenKind::Literal(Lit::Number(3.)), 14..15),
				Token::WHITESPACE,
				Token::new(TokenKind::CloseDelim(Delim::Brace), 16..17),
			]
		);
	}
}
