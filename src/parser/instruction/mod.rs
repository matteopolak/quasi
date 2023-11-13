pub mod assign;
pub mod r#if;
pub mod reassign;
pub mod r#while;

use crate::{
	error::ParseError,
	lexer::{Symbol, Token, TokenKind},
	parser::Expr,
	span::Span,
};

use super::{Parse, TokenStream};

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
pub enum InstructionKind {
	Assign(assign::Assign),
	Reassign(reassign::Reassign),
	If(r#if::If),
	Print { value: Expr },
	While(r#while::While),
}

impl Parse for Instruction {
	fn parse(tokens: &mut TokenStream) -> Result<Self, ParseError>
	where
		Self: Sized,
	{
		Ok(match tokens.peek() {
			Some(Token {
				kind: TokenKind::Keyword(Symbol::Let),
				span,
			}) => Instruction {
				span: span.to(&tokens.span()),
				kind: InstructionKind::Assign(assign::Assign::parse(tokens)?),
			},
			Some(Token {
				kind: TokenKind::Ident(..),
				span,
			}) => Instruction {
				span: span.to(&tokens.span()),
				kind: InstructionKind::Reassign(reassign::Reassign::parse(tokens)?),
			},
			Some(Token {
				kind: TokenKind::Keyword(Symbol::If),
				span,
			}) => Instruction {
				span: span.to(&tokens.span()),
				kind: InstructionKind::If(r#if::If::parse(tokens)?),
			},
			Some(Token {
				kind: TokenKind::Keyword(Symbol::Print),
				span,
			}) => {
				let start = span.clone();

				tokens.next();

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
			Some(Token {
				kind: TokenKind::Keyword(Symbol::While),
				span,
			}) => Instruction {
				span: span.to(&tokens.span()),
				kind: InstructionKind::While(r#while::While::parse(tokens)?),
			},
			_ => {
				return Err(ParseError::expected(
					vec![
						TokenKind::Keyword(Symbol::Let),
						TokenKind::Keyword(Symbol::If),
						TokenKind::Keyword(Symbol::Print),
					],
					tokens.next().map(|t| t.kind),
				));
			}
		})
	}
}
