use std::io::Write;

use super::Scope;
use crate::{
	error::RuntimeError,
	lexer::{BoolOp, Cmp, Lit, Op},
	parser::{expr::ExprOp, Expr},
	span::Span,
	Error,
};

impl Expr {
	pub fn resolve(
		&self,
		mut scope: Scope,
		out: &mut impl Write,
		span: &Span,
	) -> Result<(Scope, Lit), Error> {
		let lit = match self {
			Self::Lit(lit) => lit.clone(),
			Self::Ident(ident) => {
				return scope
					.get(ident)
					.cloned()
					.map(|lit| (scope, lit))
					.map_err(|e| e.with_span(span.clone()))
			}
			Self::Grouped(expr) => return expr.resolve(scope, out, span),
			Self::FnCall(call) => {
				let (lit, scope) = match call.execute(scope, out, span) {
					Err(Error::Runtime {
						source: RuntimeError::Return { value, scope },
						..
					}) => (Some(value), scope),
					o => (None, o?),
				};

				if let Some(lit) = lit {
					return Ok((scope, lit));
				}

				return Err(RuntimeError::MissingReturn.with_span(span.clone()));
			}
			Self::Op { op, lhs, rhs } => {
				let (s, lhs) = lhs.resolve(scope, out, span)?;
				let (s, rhs) = rhs.resolve(s, out, span)?;

				scope = s;

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
						Cmp::EqEq => Lit::Bool((lhs - rhs).abs() < f64::EPSILON),
						Cmp::Ne => Lit::Bool((lhs - rhs).abs() >= f64::EPSILON),
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
						return Err(RuntimeError::InvalidOperation { lhs, op: *op, rhs }
							.with_span(span.clone()));
					}
				}
			}
		};

		Ok((scope, lit))
	}
}
