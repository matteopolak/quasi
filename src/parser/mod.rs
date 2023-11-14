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

		ts.dedup_whitespace();
		ts.skip_whitespace();
		ts
	}

	/// Merges whitespace tokens into a single token
	/// and preserves the span appropriately.
	fn dedup_whitespace(&mut self) {
		let mut tokens = VecDeque::new();
		let mut last = None;

		for token in self.tokens.drain(..) {
			if let Some(Token {
				kind: TokenKind::Whitespace,
				..
			}) = last
			{
				if let TokenKind::Whitespace = token.kind {
					last = Some(Token {
						kind: TokenKind::Whitespace,
						span: last.unwrap().span.merge(&token.span),
					});

					continue;
				}
			}

			if let Some(last) = last {
				tokens.push_back(last);
			}

			last = Some(token);
		}

		if let Some(last) = last {
			tokens.push_back(last);
		}

		self.tokens = tokens;
	}

	pub fn peek(&self) -> Option<&Token> {
		self.tokens.get(0)
	}

	pub fn nth(&self, n: usize) -> Option<&Token> {
		self.tokens.get(n)
	}

	pub fn next(&mut self) -> Option<Token> {
		let next = self.tokens.pop_front();

		if let Some(next) = next.clone() {
			self.offset += next.span.len();
			self.used.push(next);
		}

		self.skip_whitespace();
		next
	}

	pub fn ret(&mut self, token: Token) {
		self.offset -= token.span.len();
		self.tokens.push_front(token);
		self.skip_whitespace();
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

	pub fn tokens(&self, span: &Span) -> Vec<Token> {
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

#[cfg(test)]
mod test {
	use crate::{
		lexer::{Cmp, Delim, Ident, Lit, Symbol},
		parser::{
			body::Body,
			expr::ExprOp,
			instruction::{assign::Assign, r#while::While, reassign::Reassign, InstructionKind},
		},
	};

	use super::*;

	fn parse(stream: TokenStream) -> Vec<InstructionKind> {
		let instructions = instructionify(stream)
			.map(|i| i.map(|i| i.kind))
			.collect::<Result<Vec<_>, _>>();

		assert!(instructions.is_ok());

		instructions.unwrap()
	}

	#[test]
	fn test_assign() {
		let tokens = TokenStream::new(
			vec![
				Token::new(TokenKind::Keyword(Symbol::Let), 0..3),
				Token::new(TokenKind::Ident(Ident::new("a")), 0..1),
				Token::new(TokenKind::Eq, 1..2),
				Token::new(TokenKind::Literal(Lit::Number(1.)), 2..3),
				Token::new(TokenKind::Semi, 3..4),
			]
			.into(),
		);

		let result = parse(tokens);

		assert_eq!(
			result,
			vec![InstructionKind::Assign(Assign {
				ident: Ident::new("a"),
				value: Expr::Lit(Lit::Number(1.)),
			})],
		);
	}

	#[test]
	fn test_reassign() {
		let tokens = TokenStream::new(
			vec![
				Token::new(TokenKind::Ident(Ident::new("a")), 0..1),
				Token::new(TokenKind::Eq, 1..2),
				Token::new(TokenKind::Literal(Lit::Number(1.)), 2..3),
				Token::new(TokenKind::Semi, 3..4),
			]
			.into(),
		);

		let result = parse(tokens);

		assert_eq!(
			result,
			vec![InstructionKind::Reassign(Reassign {
				ident: Ident::new("a"),
				value: Expr::Lit(Lit::Number(1.)),
			})],
		);
	}

	#[test]
	fn test_while() {
		let tokens = TokenStream::new(
			vec![
				Token::new(TokenKind::Keyword(Symbol::While), 0..5),
				Token::new(TokenKind::Literal(Lit::Number(1.)), 5..6),
				Token::new(TokenKind::Cmp(Cmp::Lt), 6..7),
				Token::new(TokenKind::Literal(Lit::Number(2.)), 7..8),
				Token::new(TokenKind::OpenDelim(Delim::Bracket), 8..9),
				Token::new(TokenKind::CloseDelim(Delim::Bracket), 10..11),
			]
			.into(),
		);

		let result = parse(tokens);

		assert_eq!(
			result,
			vec![InstructionKind::While(While {
				cond: Expr::Op {
					op: ExprOp::Cmp(Cmp::Lt),
					lhs: Box::new(Expr::Lit(Lit::Number(1.))),
					rhs: Box::new(Expr::Lit(Lit::Number(2.))),
				},
				body: Box::new(Body {
					instructions: vec![]
				}),
			})],
		);
	}
}
