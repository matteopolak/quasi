use crate::{
	error::ParseError,
	lexer::{Ident, TokenKind},
	parser::{Expr, Parse, TokenStream},
};

#[derive(Debug, PartialEq, Eq)]
pub struct Reassign {
	pub ident: Ident,
	pub value: Expr,
}

impl Parse for Reassign {
	/// Parses `<ident> = <expr>;`
	fn parse(tokens: &mut TokenStream) -> Result<Self, ParseError>
	where
		Self: Sized,
	{
		let ident = match tokens.next() {
			Some(token) => match token.kind {
				TokenKind::Ident(ident) => ident,
				token => {
					return Err(ParseError::expected(
						vec![TokenKind::Ident(Ident::default())],
						Some(token),
					))
				}
			},
			None => {
				return Err(ParseError::expected(
					vec![TokenKind::Ident(Ident::default())],
					None,
				))
			}
		};

		let Some(token) = tokens.next() else {
			return Err(ParseError::expected(vec![TokenKind::Eq], None));
		};

		match token.kind {
			TokenKind::Eq => (),
			other => return Err(ParseError::expected(vec![TokenKind::Eq], Some(other))),
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
