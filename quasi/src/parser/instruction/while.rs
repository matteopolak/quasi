use crate::{
	error::ParseError,
	expect,
	lexer::Symbol,
	parser::{body::Body, Expr, Parse, TokenStream},
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct While {
	pub cond: Expr,
	pub body: Box<Body>,
}

impl Parse for While {
	fn parse(tokens: &mut TokenStream) -> Result<Self, ParseError>
	where
		Self: Sized,
	{
		expect!(tokens, [Keyword(Symbol::While) => Keyword(Symbol::While)]);

		let cond = Expr::parse(tokens)?;
		let body = Body::parse(tokens)?;

		Ok(Self {
			cond,
			body: Box::new(body),
		})
	}
}
