use crate::{
	error::ParseError,
	lexer::{Symbol, Token, TokenKind},
	parser::{body::Body, Expr, Parse, TokenStream},
};

#[derive(Debug, PartialEq, Eq)]
pub struct If {
	pub cond: Expr,
	pub body: Box<Body>,
	pub el: Option<Box<Body>>,
}

impl Parse for If {
	fn parse(tokens: &mut TokenStream) -> Result<Self, ParseError>
	where
		Self: Sized,
	{
		match tokens.next().map(|t| t.kind) {
			Some(TokenKind::Keyword(Symbol::If)) => (),
			other => {
				return Err(ParseError::expected(
					vec![TokenKind::Keyword(Symbol::If)],
					other,
				))
			}
		};

		let cond = Expr::parse(tokens)?;
		let body = Body::parse(tokens)?;

		let el = if let Some(TokenKind::Keyword(Symbol::Else)) = tokens.peek().map(Token::kind) {
			tokens.next();

			Some(Box::new(Body::parse(tokens)?))
		} else {
			None
		};

		Ok(Self {
			cond,
			body: Box::new(body),
			el,
		})
	}
}
