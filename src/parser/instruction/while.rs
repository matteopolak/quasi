use crate::{
	error::ParseError,
	lexer::{Symbol, TokenKind},
	parser::{body::Body, Expr, Parse, TokenStream},
};

#[derive(Debug, PartialEq, Eq)]
pub struct While {
	pub cond: Expr,
	pub body: Box<Body>,
}

impl Parse for While {
	fn parse(tokens: &mut TokenStream) -> Result<Self, ParseError>
	where
		Self: Sized,
	{
		match tokens.next().map(|t| t.kind) {
			Some(TokenKind::Keyword(Symbol::While)) => (),
			other => {
				return Err(ParseError::expected(
					vec![TokenKind::Keyword(Symbol::While)],
					other,
				))
			}
		};

		let cond = Expr::parse(tokens)?;
		let body = Body::parse(tokens)?;

		Ok(Self {
			cond,
			body: Box::new(body),
		})
	}
}
