use super::Scope;
use crate::{
	error::RuntimeError,
	lexer::{BoolOp, Cmp, Lit, Op},
	parser::{expr::ExprOp, Expr},
};

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
						return Err(RuntimeError::InvalidOperation { lhs, op: *op, rhs });
					}
				}
			}
		})
	}
}
