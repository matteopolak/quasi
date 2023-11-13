pub mod body;
pub mod expr;
pub mod instruction;

use std::{collections::VecDeque, fmt};

pub use expr::Expr;
pub use instruction::Instruction;

use crate::{
	error::ParseError,
	lexer::{Token, TokenKind},
	span::Span,
	Error,
};

pub struct TokenStream {
	tokens: VecDeque<Token>,
	used: Vec<Token>,
	offset: usize,
}

impl fmt::Debug for TokenStream {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		self.tokens.fmt(f)
	}
}

impl TokenStream {
	pub fn new(tokens: VecDeque<Token>) -> Self {
		let mut ts = Self {
			tokens,
			offset: 0,
			used: vec![],
		};

		ts.skip_whitespace();
		ts
	}

	pub fn peek(&self) -> Option<&Token> {
		self.tokens.get(0)
	}

	pub fn next(&mut self) -> Option<Token> {
		let next = self.tokens.pop_front();

		// TODO: Remove this clone
		if let Some(next) = next.clone() {
			self.used.push(next);
		}

		self.offset += 1;
		self.skip_whitespace();
		next
	}

	pub fn skip_whitespace(&mut self) {
		while let Some(Token {
			kind: TokenKind::Whitespace,
			..
		}) = self.peek()
		{
			// SAFETY: We just checked that the token is whitespace
			let token = unsafe { self.tokens.pop_front().unwrap_unchecked() };
			self.used.push(token);
			self.offset += 1;
		}
	}

	pub fn is_empty(&self) -> bool {
		self.tokens.is_empty()
	}

	pub fn span(&self) -> Span {
		Span::new(self.offset..self.offset + self.tokens.len())
	}

	pub fn tokens(&self, span: Span) -> Vec<Token> {
		self.used
			.iter()
			.chain(self.tokens.iter())
			.filter(|t| span.overlaps(&t.span))
			.cloned()
			.collect()
	}
}

impl Iterator for TokenStream {
	type Item = Token;

	fn next(&mut self) -> Option<Self::Item> {
		TokenStream::next(self)
	}
}

impl FromIterator<Token> for TokenStream {
	fn from_iter<T: IntoIterator<Item = Token>>(iter: T) -> Self {
		Self::new(VecDeque::from_iter(iter))
	}
}

pub trait Parse {
	fn parse(tokens: &mut TokenStream) -> Result<Self, ParseError>
	where
		Self: Sized;
}

pub struct Instructionify {
	tokens: TokenStream,
}

impl Instructionify {
	pub fn new(tokens: TokenStream) -> Self {
		Self { tokens }
	}
}

impl Iterator for Instructionify {
	type Item = Result<Instruction, Error>;

	fn next(&mut self) -> Option<Self::Item> {
		if self.tokens.is_empty() {
			return None;
		}

		Some(Instruction::parse(&mut self.tokens).map_err(|e| e.with_span(self.tokens.span())))
	}
}

pub fn instructionify(tokens: TokenStream) -> Instructionify {
	Instructionify::new(tokens)
}
