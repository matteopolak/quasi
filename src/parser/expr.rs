use std::fmt;

use crate::{
	error,
	lexer::{BoolOp, Cmp, Delim, Ident, Lit, Op, Token, TokenKind},
};

use super::{Parse, TokenStream};

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
	Lit(Lit),
	Ident(Ident),
	/// An expression that is grouped by parentheses.
	Grouped(Box<Self>),
	Op {
		op: ExprOp,
		lhs: Box<Self>,
		rhs: Box<Self>,
	},
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ExprOp {
	Op(Op),
	Cmp(Cmp),
	BoolOp(BoolOp),
}

impl fmt::Display for ExprOp {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Op(op) => write!(f, "{op}"),
			Self::Cmp(cmp) => write!(f, "{cmp}"),
			Self::BoolOp(op) => write!(f, "{op}"),
		}
	}
}

impl Expr {
	fn parse_cmp(tokens: &mut TokenStream) -> Result<Self, error::ParseError> {
		let mut expr = Self::parse_add_sub(tokens)?;

		while let Some(Token {
			kind: TokenKind::Cmp(op),
			..
		}) = tokens.peek()
		{
			let op = *op;

			tokens.next();

			let rhs = Self::parse_add_sub(tokens)?;

			expr = Self::Op {
				op: ExprOp::Cmp(op),
				lhs: Box::new(expr),
				rhs: Box::new(rhs),
			}
		}

		Ok(expr)
	}

	fn parse_add_sub(tokens: &mut TokenStream) -> Result<Self, error::ParseError> {
		let mut expr = Self::parse_term(tokens)?;

		while let Some(Token {
			kind: TokenKind::Op(op @ (Op::Add | Op::Sub)),
			..
		}) = tokens.peek()
		{
			let op = *op;

			tokens.next();

			let rhs = Self::parse_term(tokens)?;

			expr = Self::Op {
				op: ExprOp::Op(op),
				lhs: Box::new(expr),
				rhs: Box::new(rhs),
			}
		}

		Ok(expr)
	}

	fn parse_term(tokens: &mut TokenStream) -> Result<Self, error::ParseError> {
		let mut expr = Self::parse_exponent(tokens)?;

		while let Some(Token {
			kind: TokenKind::Op(op @ (Op::Mul | Op::Div | Op::Mod)),
			..
		}) = tokens.peek()
		{
			let op = *op;

			tokens.next();

			let rhs = Self::parse_exponent(tokens)?;

			expr = Self::Op {
				op: ExprOp::Op(op),
				lhs: Box::new(expr),
				rhs: Box::new(rhs),
			}
		}

		Ok(expr)
	}

	fn parse_exponent(tokens: &mut TokenStream) -> Result<Self, error::ParseError> {
		let mut expr = Self::parse_factor(tokens)?;

		while let Some(Token {
			kind: TokenKind::Op(Op::Exp),
			..
		}) = tokens.peek()
		{
			tokens.next();

			let rhs = Self::parse_factor(tokens)?;

			expr = Self::Op {
				op: ExprOp::Op(Op::Exp),
				lhs: Box::new(expr),
				rhs: Box::new(rhs),
			}
		}

		Ok(expr)
	}

	fn parse_factor(tokens: &mut TokenStream) -> Result<Self, error::ParseError> {
		match tokens.next().map(|t| t.kind) {
			Some(TokenKind::Literal(lit)) => Ok(Self::Lit(lit)),
			Some(TokenKind::Ident(ident)) => Ok(Self::Ident(ident)),
			Some(TokenKind::OpenDelim(Delim::Paren)) => {
				let expr = Self::parse(tokens)?;

				match tokens.next().map(|t| t.kind) {
					Some(TokenKind::CloseDelim(Delim::Paren)) => (),
					token => {
						return Err(error::ParseError::expected(
							vec![TokenKind::CloseDelim(Delim::Paren)],
							token,
						))
					}
				}

				Ok(Self::Grouped(Box::new(expr)))
			}
			other => Err(error::ParseError::expected(
				vec![
					TokenKind::Literal(Lit::default()),
					TokenKind::Ident(Ident::default()),
					TokenKind::OpenDelim(Delim::Paren),
				],
				other,
			)),
		}
	}
}

impl Parse for Expr {
	/// Parses an expression, which is made up of literals, identifiers, and operators.
	fn parse(tokens: &mut TokenStream) -> Result<Self, error::ParseError> {
		let mut expr = Self::parse_cmp(tokens)?;

		while let Some(Token {
			kind: TokenKind::BoolOp(op),
			..
		}) = tokens.peek()
		{
			let op = *op;

			tokens.next();

			let rhs = Self::parse_cmp(tokens)?;

			expr = Self::Op {
				op: ExprOp::BoolOp(op),
				lhs: Box::new(expr),
				rhs: Box::new(rhs),
			}
		}

		Ok(expr)
	}
}
