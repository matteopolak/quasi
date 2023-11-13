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
