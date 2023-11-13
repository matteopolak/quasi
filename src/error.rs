use std::{fmt, io};

use crate::{
	executor::scope::Value,
	lexer::{Ident, Lit, Token, TokenKind},
	parser::{expr::ExprOp, TokenStream},
	span::Span,
};

#[derive(Debug)]
pub enum Error {
	Io(io::Error),
	Parse { source: ParseError, span: Span },
	Runtime { source: RuntimeError, span: Span },
	Lexer { source: LexerError, span: Span },
}

impl From<io::Error> for Error {
	fn from(e: io::Error) -> Self {
		Self::Io(e)
	}
}

impl Error {
	#[must_use]
	pub fn format(&self, input: &[u8]) -> String {
		match self {
			Self::Io(e) => format!("io error: {e}"),
			Self::Parse { source, span } => {
				format!("parse error: {}\n{}", source, span.highlight(input))
			}
			Self::Runtime { source, span } => {
				format!("runtime error: {}\n{}", source, span.highlight(input))
			}
			Self::Lexer { source, span } => {
				format!("lexer error: {}\n{}", source, span.highlight(input))
			}
		}
	}

	#[must_use]
	pub fn offset(self, offset: usize) -> Self {
		match self {
			Self::Parse { source, span } => Self::Parse {
				source,
				span: span.offset(offset),
			},
			Self::Runtime { source, span } => Self::Runtime {
				source,
				span: span.offset(offset),
			},
			Self::Lexer { source, span } => Self::Lexer {
				source,
				span: span.offset(offset),
			},
			Self::Io(..) => self,
		}
	}

	#[must_use]
	pub fn span(&self) -> &Span {
		match self {
			Self::Parse { span, .. } | Self::Runtime { span, .. } | Self::Lexer { span, .. } => {
				span
			}
			Self::Io(..) => unreachable!(),
		}
	}

	#[must_use]
	pub fn tokens(&self, stream: &TokenStream) -> Vec<Token> {
		stream.tokens(self.span())
	}
}

#[derive(Debug)]
pub enum ParseError {
	UnexpectedToken {
		expected: Vec<TokenKind>,
		found: TokenKind,
	},
	UnexpectedType {
		expected: Lit,
		found: Lit,
	},
}

impl fmt::Display for ParseError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::UnexpectedToken { expected, found } => {
				if expected.len() == 1 {
					write!(f, "expected `{}`, found `{}`", expected[0], found)
				} else {
					write!(
						f,
						"expected one of {}, found `{}`",
						expected
							.iter()
							.map(|t| format!("`{t}`"))
							.collect::<Vec<_>>()
							.join(", "),
						found
					)
				}
			}
			Self::UnexpectedType { expected, found } => {
				write!(f, "expected `{expected}`, found `{found}`")
			}
		}
	}
}

impl ParseError {
	pub fn expected(expected: Vec<TokenKind>, found: Option<TokenKind>) -> Self {
		Self::UnexpectedToken {
			expected,
			found: found.unwrap_or(TokenKind::Eof),
		}
	}

	pub fn with_span(self, span: Span) -> Error {
		Error::Parse { source: self, span }
	}
}

#[derive(Debug)]
pub enum RuntimeError {
	UnknownVariable(Ident),
	InvalidOperation { op: ExprOp, lhs: Value, rhs: Value },
	InvalidCondition { cond: Value },
}

impl fmt::Display for RuntimeError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::UnknownVariable(ident) => write!(f, "unknown variable `{ident}`"),
			Self::InvalidOperation { op, lhs, rhs } => {
				write!(f, "invalid operation on `{lhs}` {op} `{rhs}`")
			}
			Self::InvalidCondition { cond } => {
				write!(f, "invalid condition: '{cond}'")
			}
		}
	}
}

impl RuntimeError {
	pub fn with_span(self, span: Span) -> Error {
		Error::Runtime { source: self, span }
	}
}

#[derive(Debug)]
pub enum LexerError {
	UnexpectedToken { token: char },
	InvalidNumber,
	UnexpectedEof,
}

impl fmt::Display for LexerError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::UnexpectedToken { token } => {
				write!(f, "unexpected token `{token}`")
			}
			Self::InvalidNumber => write!(f, "invalid number"),
			Self::UnexpectedEof => write!(f, "unexpected end of file"),
		}
	}
}

impl LexerError {
	pub fn with_span(self, span: Span) -> Error {
		Error::Lexer { source: self, span }
	}
}
