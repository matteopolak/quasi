use crate::{
	error::ParseError,
	expect,
	lexer::Symbol,
	parser::{body::Body, Expr, Parse, TokenStream},
};

use super::Instruction;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct For {
	pub setup: Box<Instruction>,
	pub cond: Expr,
	pub update: Box<Instruction>,
	pub body: Box<Body>,
}

impl Parse for For {
	fn parse(tokens: &mut TokenStream) -> Result<Self, ParseError>
	where
		Self: Sized,
	{
		expect!(tokens, [Keyword(Symbol::For) => Keyword(Symbol::For)]);

		let setup = Instruction::parse(tokens)?;
		let cond = Expr::parse(tokens)?;
		expect!(tokens, [Semi => Semi]);

		let update = Instruction::parse(tokens)?;
		let body = Body::parse(tokens)?;

		Ok(Self {
			setup: Box::new(setup),
			cond,
			update: Box::new(update),
			body: Box::new(body),
		})
	}
}
