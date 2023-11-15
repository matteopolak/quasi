use crate::{
	error::ParseError,
	expect,
	lexer::{Symbol, Token, TokenKind},
	parser::{body::Body, Expr, Parse, TokenStream},
};

#[derive(Debug, PartialEq, Eq, Clone)]
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
		expect!(tokens, [Keyword(Symbol::If) => Keyword(Symbol::If)]);

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
