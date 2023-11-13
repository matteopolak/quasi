use crate::{
	error::{self, RuntimeError},
	lexer::{BoolOp, Cmp, Delim, Ident, Lit, Op, Token, TokenKind},
	scope::Scope,
};

use super::{Parse, TokenStream};

#[derive(Debug)]
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

impl Expr {
	pub fn resolve(&self, scope: &Scope) -> Result<Lit, RuntimeError> {
		Ok(match self {
			Self::Lit(lit) => lit.clone(),
			Self::Ident(ident) => scope.get(ident)?.clone(),
			Self::Grouped(expr) => expr.resolve(scope)?,
			Self::Op { op, lhs, rhs } => {
				let lhs = lhs.resolve(scope)?;
				let rhs = rhs.resolve(scope)?;

				match (op, &lhs, &rhs) {
					(ExprOp::Op(op), Lit::Number(lhs), Lit::Number(rhs)) => match op {
						Op::Add => Lit::Number(lhs + rhs),
						Op::Sub => Lit::Number(lhs - rhs),
						Op::Mul => Lit::Number(lhs * rhs),
						Op::Div => Lit::Number(lhs / rhs),
						Op::Mod => Lit::Number(lhs % rhs),
						Op::Exp => Lit::Number(lhs.powf(*rhs)),
					},
					(ExprOp::Op(Op::Add), Lit::String(lhs), Lit::String(rhs)) => {
						Lit::String(lhs.clone() + rhs)
					}
					(ExprOp::Cmp(op), Lit::Number(lhs), Lit::Number(rhs)) => match op {
						Cmp::Lt => Lit::Bool(lhs < rhs),
						Cmp::Le => Lit::Bool(lhs <= rhs),
						Cmp::EqEq => Lit::Bool(lhs == rhs),
						Cmp::Ne => Lit::Bool(lhs != rhs),
						Cmp::Ge => Lit::Bool(lhs >= rhs),
						Cmp::Gt => Lit::Bool(lhs > rhs),
					},
					(ExprOp::Cmp(op), Lit::String(lhs), Lit::String(rhs)) => match op {
						Cmp::Lt => Lit::Bool(lhs < rhs),
						Cmp::Le => Lit::Bool(lhs <= rhs),
						Cmp::EqEq => Lit::Bool(lhs == rhs),
						Cmp::Ne => Lit::Bool(lhs != rhs),
						Cmp::Ge => Lit::Bool(lhs >= rhs),
						Cmp::Gt => Lit::Bool(lhs > rhs),
					},
					(ExprOp::BoolOp(op), Lit::Bool(lhs), Lit::Bool(rhs)) => match op {
						BoolOp::And => Lit::Bool(*lhs && *rhs),
						BoolOp::Or => Lit::Bool(*lhs || *rhs),
					},
					_ => {
						return Err(RuntimeError::InvalidOperation { lhs, op: *op, rhs });
					}
				}
			}
		})
	}

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
			kind: TokenKind::Op(op),
			..
		}) = tokens.peek()
		{
			match op {
				Op::Add | Op::Sub => (),
				_ => break,
			}

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
			kind: TokenKind::Op(op),
			..
		}) = tokens.peek()
		{
			match op {
				Op::Mul | Op::Div | Op::Mod => (),
				_ => break,
			}

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
