use crate::{
	error::ParseError,
	lexer::{Ident, Symbol, Token, TokenKind},
	parser::Expr,
	span::Span,
};

use super::{body::Body, Parse, TokenStream};

#[derive(Debug)]
pub struct Instruction {
	pub kind: InstructionKind,
	pub span: Span,
}

impl Instruction {
	#[cfg(test)]
	pub fn new(kind: InstructionKind) -> Self {
		Self {
			kind,
			span: Span::default(),
		}
	}
}

#[derive(Debug)]
pub enum InstructionKind {
	Assign {
		ident: Ident,
		value: Expr,
	},
	Reassign {
		ident: Ident,
		value: Expr,
	},
	If {
		cond: Expr,
		body: Box<Body>,
		el: Option<Box<Body>>,
	},
	Print {
		value: Expr,
	},
	While {
		cond: Expr,
		body: Box<Body>,
	},
}

impl Parse for Instruction {
	fn parse(tokens: &mut TokenStream) -> Result<Self, ParseError>
	where
		Self: Sized,
	{
		Ok(match tokens.next().map(|t| t.kind) {
			Some(TokenKind::Keyword(Symbol::Let)) => {
				let token = match tokens.next() {
					Some(token) => token,
					None => {
						return Err(ParseError::expected(
							vec![TokenKind::Ident(Ident::default())],
							None,
						))
					}
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
				let last = match tokens.next() {
					Some(token) => token,
					None => return Err(ParseError::expected(vec![TokenKind::Semi], None)),
				};

				match last.kind {
					TokenKind::Semi => (),
					other => return Err(ParseError::expected(vec![TokenKind::Semi], Some(other))),
				};

				Instruction {
					kind: InstructionKind::Assign { ident, value },
					span: token.span.to(last.span()),
				}
			}
			Some(TokenKind::Ident(ident)) => {
				let token = match tokens.next() {
					Some(token) => token,
					None => return Err(ParseError::expected(vec![TokenKind::Eq], None)),
				};

				match token.kind {
					TokenKind::Eq => (),
					other => return Err(ParseError::expected(vec![TokenKind::Eq], Some(other))),
				};

				let value = Expr::parse(tokens)?;
				let last = match tokens.next() {
					Some(token) => token,
					None => return Err(ParseError::expected(vec![TokenKind::Semi], None)),
				};

				match last.kind {
					TokenKind::Semi => (),
					other => return Err(ParseError::expected(vec![TokenKind::Semi], Some(other))),
				};

				Instruction {
					kind: InstructionKind::Reassign { ident, value },
					span: token.span.to(last.span()),
				}
			}
			Some(TokenKind::Keyword(Symbol::If)) => {
				let start = tokens.span();
				let cond = Expr::parse(tokens)?;
				let body = Body::parse(tokens)?;

				let el = if let Some(TokenKind::Keyword(Symbol::Else)) =
					tokens.peek().map(Token::kind)
				{
					tokens.next();

					Some(Box::new(Body::parse(tokens)?))
				} else {
					None
				};

				let end = tokens.span();

				Instruction {
					kind: InstructionKind::If {
						cond,
						body: Box::new(body),
						el,
					},
					span: start.to(&end),
				}
			}
			Some(TokenKind::Keyword(Symbol::Print)) => {
				let start = tokens.span();
				let value = Expr::parse(tokens)?;

				match tokens.next().map(|t| t.kind) {
					Some(TokenKind::Semi) => (),
					other => return Err(ParseError::expected(vec![TokenKind::Semi], other)),
				};

				Instruction {
					kind: InstructionKind::Print { value },
					span: start.to(&tokens.span()),
				}
			}
			Some(TokenKind::Keyword(Symbol::While)) => {
				let start = tokens.span();
				let cond = Expr::parse(tokens)?;
				let body = Body::parse(tokens)?;

				let end = tokens.span();

				Instruction {
					kind: InstructionKind::While {
						cond,
						body: Box::new(body),
					},
					span: start.to(&end),
				}
			}
			token => {
				return Err(ParseError::expected(
					vec![
						TokenKind::Keyword(Symbol::Let),
						TokenKind::Keyword(Symbol::If),
						TokenKind::Keyword(Symbol::Print),
					],
					token,
				));
			}
		})
	}
}
