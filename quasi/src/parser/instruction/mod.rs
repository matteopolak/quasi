pub mod assign;
pub mod r#fn;
pub mod r#for;
pub mod r#if;
pub mod reassign;
pub mod r#while;

use crate::{
	error::ParseError,
	expect,
	lexer::{Delim, Ident, Symbol, Token, TokenKind},
	parser::Expr,
	span::Span,
};

use super::{Parse, TokenStream};

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InstructionKind {
	Assign(assign::Assign),
	Reassign(reassign::Reassign),
	If(r#if::If),
	Print { value: Expr },
	While(r#while::While),
	Fn(r#fn::Fn),
	FnCall(r#fn::FnCall),
	For(r#for::For),
	Return(Expr),
}

pub struct InstructionParseOptions {
	pub consume_semi: bool,
}

impl From<InstructionParseOptions> for reassign::ReassignParseOptions {
	fn from(options: InstructionParseOptions) -> Self {
		Self {
			consume_semi: options.consume_semi,
		}
	}
}

impl Parse for Instruction {
	type Options = InstructionParseOptions;

	fn parse(tokens: &mut TokenStream) -> Result<Self, ParseError>
	where
		Self: Sized,
	{
		Self::parse_with(tokens, InstructionParseOptions { consume_semi: true })
	}

	#[allow(clippy::too_many_lines)]
	fn parse_with(tokens: &mut TokenStream, options: Self::Options) -> Result<Self, ParseError>
	where
		Self: Sized,
	{
		Ok(match tokens.peek() {
					Some(Token {
						kind: TokenKind::Keyword(Symbol::Return),
						span,
					}) => {
						let start = span.clone();

						tokens.next();

						let value = Expr::parse(tokens)?;

						if options.consume_semi {
							expect!(tokens, [Semi => Semi]);
						}

						Instruction {
							span: start.to(&tokens.span()),
							kind: InstructionKind::Return(value),
						}
					}
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
					})
					// In order for `{ident} = {expr}` to be a reassignment, the next token
					// cannot be `(`, because that would be a function call.
					if !(*tokens)
						.nth(1)
						.is_some_and(|t| t.kind == TokenKind::OpenDelim(Delim::Paren)) =>
					{
						Instruction {
							span: span.to(&tokens.span()),
							kind: InstructionKind::Reassign(reassign::Reassign::parse_with(tokens, options.into())?),
						}
					}
					Some(Token {
						kind: TokenKind::Ident(..),
						span,
					}) => {
						let i = Instruction {
							span: span.to(&tokens.span()),
							kind: InstructionKind::FnCall(r#fn::FnCall::parse(tokens)?),
						};

						if options.consume_semi {
							expect!(tokens, [Semi => Semi]);
						}

						i
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

						if options.consume_semi {
							expect!(tokens, [Semi => Semi]);
						}

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
					Some(Token {
						kind: TokenKind::Keyword(Symbol::Fn),
						span,
					}) => Instruction {
						span: span.to(&tokens.span()),
						kind: InstructionKind::Fn(r#fn::Fn::parse(tokens)?),
					},
					Some(Token {
						kind: TokenKind::Keyword(Symbol::For),
						span,
					}) => Instruction {
						span: span.to(&tokens.span()),
						kind: InstructionKind::For(r#for::For::parse(tokens)?),
					},
					_ => {
						return Err(ParseError::expected(
							vec![
								TokenKind::Keyword(Symbol::Let),
								TokenKind::Keyword(Symbol::If),
								TokenKind::Keyword(Symbol::Print),
								TokenKind::Keyword(Symbol::While),
								TokenKind::Ident(Ident::default()),
								TokenKind::Keyword(Symbol::For),
								TokenKind::Keyword(Symbol::Fn),
							],
							tokens.next().map(|t| t.kind),
						));
					}
				})
	}
}
