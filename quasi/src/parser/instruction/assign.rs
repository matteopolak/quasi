use crate::{
	error::ParseError,
	expect,
	lexer::{Ident, Symbol, TokenKind},
	parser::{Expr, Parse, TokenStream},
};

#[derive(Debug, PartialEq, Eq, Clone)]
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
		expect!(tokens, [Keyword(Symbol::Let) => Keyword(Symbol::Let)]);

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

		expect!(tokens, [Eq => Eq]);

		let value = Expr::parse(tokens)?;

		expect!(tokens, [Semi => Semi]);

		Ok(Self { ident, value })
	}
}
