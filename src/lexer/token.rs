use std::{cmp::Ordering, fmt, hint::unreachable_unchecked, ops::Deref};

use super::{
	util::{is_gap, is_whitespace},
	Decode,
};
use crate::{error, span::Span};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
	/// `=`
	Eq,
	/// `==`, `!=`, `<=`, `>=`, `<`, `>`
	Cmp(Cmp),
	/// `+`, `-`, `/`, `*`, `%`, `**`
	Op(Op),
	BoolOp(BoolOp),
	/// `"hello"`, `15`, ...
	Literal(Lit),
	/// `let`, `if`, ...
	Keyword(Symbol),
	Ident(Ident),
	/// ` `, `\n`, `\r`, `\t`
	Whitespace,
	/// `;`
	Semi,
	/// `{`
	OpenDelim(Delim),
	/// `}`
	CloseDelim(Delim),
	/// End of file
	Eof,
}

impl TokenKind {
	pub fn with_span(self, span: Span) -> Token {
		Token { kind: self, span }
	}
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
	pub kind: TokenKind,
	pub span: Span,
}

impl Deref for Token {
	type Target = TokenKind;

	fn deref(&self) -> &Self::Target {
		&self.kind
	}
}

#[derive(Debug, PartialEq, Eq, Default, Clone, Copy)]
pub enum Delim {
	#[default]
	Paren,
	Brace,
	Bracket,
}

impl Token {
	pub fn is_whitespace(&self) -> bool {
		matches!(self.kind, TokenKind::Whitespace)
	}

	pub fn is_op(&self) -> bool {
		matches!(
			self.kind,
			TokenKind::Op(_) | TokenKind::BoolOp(_) | TokenKind::Cmp(_)
		)
	}

	/// Whether the token is a delimiter that ends an expression.
	pub fn is_expr_end(&self) -> bool {
		matches!(
			self.kind,
			TokenKind::Semi | TokenKind::OpenDelim(_) | TokenKind::CloseDelim(_)
		)
	}

	pub fn kind(&self) -> &TokenKind {
		&self.kind
	}

	pub fn span(&self) -> &Span {
		&self.span
	}
}

impl fmt::Display for Token {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
		self.kind.fmt(f)
	}
}

impl fmt::Display for TokenKind {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Eq => write!(f, "="),
			Self::Cmp(cmp) => cmp.fmt(f),
			Self::Op(op) => op.fmt(f),
			Self::BoolOp(op) => op.fmt(f),
			Self::Literal(lit) => lit.fmt(f),
			Self::Keyword(kw) => kw.fmt(f),
			Self::Ident(ident) => ident.fmt(f),
			Self::Semi => write!(f, ";"),
			Self::OpenDelim(d) => match d {
				Delim::Paren => write!(f, "("),
				Delim::Brace => write!(f, "{{"),
				Delim::Bracket => write!(f, "["),
			},
			Self::CloseDelim(d) => match d {
				Delim::Paren => write!(f, ")"),
				Delim::Brace => write!(f, "}}"),
				Delim::Bracket => write!(f, "]"),
			},
			Self::Eof => write!(f, "EOF"),
			Self::Whitespace => write!(f, " "),
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum BoolOp {
	#[default]
	And,
	Or,
}

impl PartialOrd for BoolOp {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}

impl Ord for BoolOp {
	fn cmp(&self, other: &Self) -> Ordering {
		match (self, other) {
			(Self::And, Self::Or) => Ordering::Greater,
			(Self::Or, Self::And) => Ordering::Less,
			_ => Ordering::Equal,
		}
	}
}

impl fmt::Display for BoolOp {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::And => write!(f, "&&"),
			Self::Or => write!(f, "||"),
		}
	}
}

#[derive(Debug, Clone)]
pub enum Lit {
	Number(f64),
	String(String),
	Bool(bool),
}

impl Default for Lit {
	fn default() -> Self {
		Self::Bool(false)
	}
}

impl fmt::Display for Lit {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Number(n) => write!(f, "{}", n),
			Self::String(s) => write!(f, "\"{}\"", s),
			Self::Bool(b) => write!(f, "{}", b),
		}
	}
}

impl Eq for Lit {}
impl PartialEq for Lit {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Self::Number(n), Self::Number(other)) => n == other,
			(Self::String(s), Self::String(other)) => s == other,
			_ => false,
		}
	}
}

impl Ord for Lit {
	fn cmp(&self, other: &Self) -> Ordering {
		match (self, other) {
			(Self::Number(n), Self::Number(other)) => {
				n.partial_cmp(other).unwrap_or(Ordering::Equal)
			}
			(Self::String(s), Self::String(other)) => s.cmp(other),
			_ => unreachable!(),
		}
	}
}

impl PartialOrd for Lit {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}

impl Decode for Lit {
	type Error = error::LexerError;

	fn decode(input: &[u8]) -> Result<(Option<Lit>, &[u8]), Self::Error> {
		let (t, r) = match input {
			[b'"', r @ ..] => {
				let mut s = String::new();
				let mut r = r;

				while !r.is_empty() {
					let (c, rr) = match r {
						[b'"', rr @ ..] => return Ok((Some(Self::String(s)), rr)),
						[c @ (b'\n' | b'\r'), ..] => {
							return Err(error::LexerError::UnexpectedToken { token: *c as char })
						}
						[b'\\', b'"', rr @ ..] => (b'"', rr),
						[b'\\', b'\\', rr @ ..] => (b'\\', rr),
						[b'\\', b'n', rr @ ..] => (b'\n', rr),
						[b'\\', b'r', rr @ ..] => (b'\r', rr),
						[b'\\', b't', rr @ ..] => (b'\t', rr),
						[b'\\', b'0', rr @ ..] => (b'\0', rr),
						[c, rr @ ..] => (*c, rr),
						// SAFETY: `r` is not empty
						[] => unsafe { unreachable_unchecked() },
					};

					s.push(c as char);
					r = rr;
				}

				return Err(error::LexerError::UnexpectedEof);
			}
			[c @ (b'0'..=b'9' | b'.'), r @ ..] => {
				let mut s = String::from(*c as char);
				let mut r = r;

				let mut has_decimal = c == &b'.';

				while !r.is_empty() {
					let (c, rr) = match r {
						[b'.', rr @ ..] if !has_decimal => {
							has_decimal = true;
							(b'.', rr)
						}
						[c @ b'0'..=b'9', rr @ ..] => (*c, rr),
						_ => break,
					};

					s.push(c as char);
					r = rr;
				}

				(
					Self::Number(s.parse().map_err(|_| error::LexerError::InvalidNumber)?),
					r,
				)
			}
			[b't', b'r', b'u', b'e', n, r @ ..] if is_gap(*n) => (Self::Bool(true), r),
			[b'f', b'a', b'l', b's', b'e', n, r @ ..] if is_gap(*n) => (Self::Bool(false), r),
			_ => return Ok((None, input)),
		};

		Ok((Some(t), r))
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Op {
	#[default]
	Add,
	Sub,
	Mul,
	Div,
	Mod,
	Exp,
}

impl PartialOrd for Op {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}

impl Ord for Op {
	fn cmp(&self, other: &Self) -> Ordering {
		match (self, other) {
			(Self::Exp, Self::Exp) => Ordering::Equal,
			(Self::Add | Self::Sub, Self::Add | Self::Sub) => Ordering::Equal,
			(Self::Mul | Self::Div | Self::Mod, Self::Mul | Self::Div | Self::Mod) => {
				Ordering::Equal
			}
			(Self::Exp, _) => Ordering::Greater,
			(Self::Mul | Self::Div | Self::Mod, Self::Add | Self::Sub) => Ordering::Greater,
			_ => Ordering::Less,
		}
	}
}

impl fmt::Display for Op {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Add => write!(f, "+"),
			Self::Sub => write!(f, "-"),
			Self::Mul => write!(f, "*"),
			Self::Div => write!(f, "/"),
			Self::Mod => write!(f, "%"),
			Self::Exp => write!(f, "**"),
		}
	}
}

impl Decode for Op {
	type Error = error::LexerError;

	fn decode(input: &[u8]) -> Result<(Option<Op>, &[u8]), Self::Error> {
		let (t, r) = match input {
			[b'+', r @ ..] => (Self::Add, r),
			[b'-', r @ ..] => (Self::Sub, r),
			[b'*', b'*', r @ ..] => (Self::Exp, r),
			[b'*', r @ ..] => (Self::Mul, r),
			[b'/', r @ ..] => (Self::Div, r),
			[b'%', r @ ..] => (Self::Mod, r),
			_ => return Ok((None, input)),
		};

		Ok((Some(t), r))
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Symbol {
	Let,
	If,
	Print,
	Else,
	While,
}

impl fmt::Display for Symbol {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Let => write!(f, "let"),
			Self::If => write!(f, "if"),
			Self::Print => write!(f, "print"),
			Self::Else => write!(f, "else"),
			Self::While => write!(f, "while"),
		}
	}
}

impl Decode for Symbol {
	type Error = error::LexerError;

	fn decode(input: &[u8]) -> Result<(Option<Symbol>, &[u8]), Self::Error> {
		let (t, r) = match input {
			[b'l', b'e', b't', n, r @ ..] if is_whitespace(*n) => (Self::Let, r),
			[b'i', b'f', n, r @ ..] if is_whitespace(*n) => (Self::If, r),
			[b'p', b'r', b'i', b'n', b't', n, r @ ..] if is_whitespace(*n) => (Self::Print, r),
			[b'e', b'l', b's', b'e', n, r @ ..] if is_gap(*n) => (Self::Else, r),
			[b'w', b'h', b'i', b'l', b'e', n, r @ ..] if is_whitespace(*n) => (Self::While, r),
			_ => return Ok((None, input)),
		};

		Ok((Some(t), r))
	}
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Default)]
pub struct Ident(String);

impl fmt::Display for Ident {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.0)
	}
}

impl Decode for Ident {
	type Error = error::LexerError;

	fn decode(input: &[u8]) -> Result<(Option<Ident>, &[u8]), Self::Error> {
		let (t, r) = match input {
			[c @ (b'a'..=b'z' | b'A'..=b'Z'), r @ ..] => {
				let mut s = String::from(*c as char);
				let mut r = r;

				while !r.is_empty() {
					let (c, rr) = match r {
						[c @ (b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9'), rr @ ..] => (*c, rr),
						_ => break,
					};

					s.push(c as char);
					r = rr;
				}

				(Self(s), r)
			}
			_ => return Ok((None, input)),
		};

		Ok((Some(t), r))
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default)]
pub enum Cmp {
	#[default]
	/// `<`
	Lt,
	/// `<=`
	Le,
	/// `==`
	EqEq,
	/// `!=`
	Ne,
	/// `>=`
	Ge,
	/// `>`
	Gt,
}

impl fmt::Display for Cmp {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Lt => write!(f, "<"),
			Self::Le => write!(f, "<="),
			Self::EqEq => write!(f, "=="),
			Self::Ne => write!(f, "!="),
			Self::Ge => write!(f, ">="),
			Self::Gt => write!(f, ">"),
		}
	}
}

impl Decode for Token {
	type Error = error::Error;

	fn decode(input: &[u8]) -> Result<(Option<Self>, &[u8]), Self::Error>
	where
		Self: Sized,
	{
		let (token, r) = match TokenKind::decode(input) {
			Ok((token, r)) => (token, r),
			Err(e) => {
				return Err(e.with_span(Span::new(input.len()..input.len())));
			}
		};

		match token {
			Some(token) => Ok((
				Some(token.with_span(Span::new(0..input.len() - r.len()))),
				r,
			)),
			None => Ok((None, input)),
		}
	}
}

impl Decode for TokenKind {
	type Error = error::LexerError;

	fn decode(input: &[u8]) -> Result<(Option<TokenKind>, &[u8]), Self::Error> {
		if input.is_empty() {
			return Ok((None, input));
		}

		let (t, r) = match input {
			[] => (Self::Eof, input),
			[b' ' | b'\n' | b'\r' | b'\t', r @ ..] => (TokenKind::Whitespace, r),
			[b'<', b'=', r @ ..] => (Self::Cmp(Cmp::Le), r),
			[b'=', b'=', r @ ..] => (Self::Cmp(Cmp::EqEq), r),
			[b'!', b'=', r @ ..] => (Self::Cmp(Cmp::Ne), r),
			[b'>', b'=', r @ ..] => (Self::Cmp(Cmp::Ge), r),
			[b'=', r @ ..] => (Self::Eq, r),
			[b'<', r @ ..] => (Self::Cmp(Cmp::Lt), r),
			[b'>', r @ ..] => (Self::Cmp(Cmp::Gt), r),
			[b';', r @ ..] => (Self::Semi, r),
			[b'{', r @ ..] => (Self::OpenDelim(Delim::Brace), r),
			[b'}', r @ ..] => (Self::CloseDelim(Delim::Brace), r),
			[b'(', r @ ..] => (Self::OpenDelim(Delim::Paren), r),
			[b')', r @ ..] => (Self::CloseDelim(Delim::Paren), r),
			[b'[', r @ ..] => (Self::OpenDelim(Delim::Bracket), r),
			[b']', r @ ..] => (Self::CloseDelim(Delim::Bracket), r),
			[b'&', b'&', r @ ..] => (Self::BoolOp(BoolOp::And), r),
			[b'|', b'|', r @ ..] => (Self::BoolOp(BoolOp::Or), r),
			_ => {
				if let Ok((Some(t), r)) = Op::decode(input) {
					return Ok((Some(Self::Op(t)), r));
				}

				if let Ok((Some(t), r)) = Lit::decode(input) {
					return Ok((Some(Self::Literal(t)), r));
				}

				if let Ok((Some(t), r)) = Symbol::decode(input) {
					return Ok((Some(Self::Keyword(t)), r));
				}

				if let Ok((Some(t), r)) = Ident::decode(input) {
					return Ok((Some(Self::Ident(t)), r));
				}

				return Err(error::LexerError::UnexpectedToken {
					token: input[0] as char,
				});
			}
		};

		Ok((Some(t), r))
	}
}
