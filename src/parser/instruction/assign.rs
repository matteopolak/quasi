use crate::{
	error::ParseError,
	lexer::{Ident, Symbol, TokenKind},
	parser::{Expr, Parse, TokenStream},
};

#[derive(Debug, PartialEq, Eq)]
pub struct Assign {
	pub ident: Ident,
	pub value: Expr,
}

impl Parse for Assign {
	/// Parses `let <ident> = <expr>;`
	fn parse(tokens: &mut TokenStream) -> Result<Self, ParseError>
	where
		Self: Sized,
	{
		match tokens.next().map(|t| t.kind) {
			Some(TokenKind::Keyword(Symbol::Let)) => (),
			other => {
				return Err(ParseError::expected(
					vec![TokenKind::Keyword(Symbol::Let)],
					other,
				))
			}
		};

		let Some(token) = tokens.next() else {
			return Err(ParseError::expected(
				vec![TokenKind::Ident(Ident::default())],
				None,
			));
		};

		let ident = match token.kind {
			TokenKind::Ident(ident) => ident,
			token => {
				return Err(ParseError::expected(
					vec![TokenKind::Ident(Ident::default())],
					Some(token),
				))
			}
		};

		match tokens.next().map(|t| t.kind) {
			Some(TokenKind::Eq) => (),
			other => return Err(ParseError::expected(vec![TokenKind::Eq], other)),
		};

		let value = Expr::parse(tokens)?;
		let Some(last) = tokens.next() else {
			return Err(ParseError::expected(vec![TokenKind::Semi], None));
		};

		match last.kind {
			TokenKind::Semi => (),
			other => return Err(ParseError::expected(vec![TokenKind::Semi], Some(other))),
		};

		Ok(Self { ident, value })
	}
}
