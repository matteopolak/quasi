use crate::{
	error::ParseError,
	expect,
	lexer::{Ident, TokenKind},
	parser::{Expr, Parse, TokenStream},
};

#[derive(Debug, PartialEq, Eq, Clone)]
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

		expect!(tokens, [Eq => Eq]);

		let value = Expr::parse(tokens)?;

		expect!(tokens, [Semi => Semi]);

		Ok(Self { ident, value })
	}
}
