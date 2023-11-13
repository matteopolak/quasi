use crate::{
	lexer::{Ident, Lit, Token, TokenKind},
	parser::{expr::ExprOp, TokenStream},
	span::Span,
};

#[derive(Debug, thiserror::Error)]
pub enum Error {
	#[error("parse error: {source:?} at {span:?}")]
	Parse { source: ParseError, span: Span },
	#[error("runtime error: {source:?} at {span:?}")]
	Runtime { source: RuntimeError, span: Span },
	#[error("lexer error: {source:?} at {span:?}")]
	Lexer { source: LexerError, span: Span },
}

impl Error {
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
		}
	}

	pub fn span(&self) -> &Span {
		match self {
			Self::Parse { span, .. } => span,
			Self::Runtime { span, .. } => span,
			Self::Lexer { span, .. } => span,
		}
	}

	pub fn tokens(&self, stream: &TokenStream) -> Vec<Token> {
		stream.tokens(self.span().clone())
	}
}

#[derive(Debug, thiserror::Error)]
pub enum ParseError {
	#[error("unexpected token: expected {expected:?}, found {found}")]
	UnexpectedToken {
		expected: Vec<TokenKind>,
		found: TokenKind,
	},
	#[error("unexpected type: expected {expected}, found {found}")]
	UnexpectedType { expected: Lit, found: Lit },
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

#[derive(Debug, thiserror::Error)]
pub enum RuntimeError {
	#[error("unknown variable: {0}")]
	UnknownVariable(Ident),
	#[error("invalid operation: {op:?}; lhs: {lhs:?}, rhs: {rhs:?}")]
	InvalidOperation { op: ExprOp, lhs: Lit, rhs: Lit },
	#[error("invalid condition: {cond:?}")]
	InvalidCondition { cond: Lit },
}

impl RuntimeError {
	pub fn with_span(self, span: Span) -> Error {
		Error::Runtime { source: self, span }
	}
}

#[derive(Debug, thiserror::Error)]
pub enum LexerError {
	#[error("unexpected token: {token:?}")]
	UnexpectedToken { token: char },
	#[error("invalid number")]
	InvalidNumber,
	#[error("unexpected end of file")]
	UnexpectedEof,
}

impl LexerError {
	pub fn with_span(self, span: Span) -> Error {
		Error::Lexer { source: self, span }
	}
}
