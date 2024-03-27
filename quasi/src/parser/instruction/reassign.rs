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

pub struct ReassignParseOptions {
	pub consume_semi: bool,
}

impl Parse for Reassign {
	type Options = ReassignParseOptions;

	/// Parses `<ident> = <expr>;`
	fn parse(tokens: &mut TokenStream) -> Result<Self, ParseError>
	where
		Self: Sized,
	{
		Self::parse_with(tokens, ReassignParseOptions { consume_semi: true })
	}

	fn parse_with(tokens: &mut TokenStream, options: Self::Options) -> Result<Self, ParseError>
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

		if options.consume_semi {
			expect!(tokens, [Semi => Semi]);
		}

		Ok(Self { ident, value })
	}
}
