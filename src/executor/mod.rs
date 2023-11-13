mod resolve;

pub mod scope;
use std::io::Write;

pub use scope::Scope;

use crate::{
	error,
	lexer::Lit,
	parser::{instruction::InstructionKind, Instruction},
	Error,
};

pub struct Executor {
	instructions: Vec<Instruction>,
}

impl FromIterator<Instruction> for Executor {
	fn from_iter<T: IntoIterator<Item = Instruction>>(iter: T) -> Self {
		Self {
			instructions: iter.into_iter().collect(),
		}
	}
}

impl Executor {
	pub fn execute(&mut self, out: &mut impl Write) -> Result<(), Error> {
		Self::_execute(
			out,
			&self.instructions,
			Scope::with_parent(Scope::default()),
		)?;

		Ok(())
	}

	fn _execute<'a>(
		out: &mut impl Write,
		instructions: &[Instruction],
		mut scope: Scope<'a>,
	) -> Result<Scope<'a>, Error> {
		for instruction in instructions {
			match instruction {
				Instruction {
					kind: InstructionKind::Assign { ident, value },
					span,
				} => {
					let value = value
						.resolve(&scope)
						.map_err(|e| e.with_span(span.clone()))?;

					scope.set(ident.clone(), value);
				}
				Instruction {
					kind: InstructionKind::If { cond, body, el },
					span,
				} => {
					let mut child_scope = Scope::with_parent(scope);

					let cond = cond
						.resolve(&child_scope)
						.map_err(|e| e.with_span(span.clone()))?;

					let cond = match cond {
						Lit::Bool(b) => b,
						other => {
							return Err(error::RuntimeError::InvalidCondition { cond: other }
								.with_span(span.clone()));
						}
					};

					if cond {
						child_scope = Self::_execute(out, &body.instructions, child_scope)?;
					} else if let Some(el) = el {
						child_scope = Self::_execute(out, &el.instructions, child_scope)?;
					}

					scope = child_scope.close();
				}
				Instruction {
					kind: InstructionKind::Print { value },
					span,
				} => match value
					.resolve(&scope)
					.map_err(|e| e.with_span(span.clone()))?
				{
					Lit::Number(n) => writeln!(out, "{}", n)?,
					Lit::String(s) => writeln!(out, "{}", s)?,
					Lit::Bool(b) => writeln!(out, "{}", b)?,
				},
				Instruction {
					kind: InstructionKind::Reassign { ident, value },
					span,
				} => {
					let value = match value.resolve(&scope) {
						Ok(value) => value,
						Err(e) => return Err(e.with_span(span.clone())),
					};

					scope
						.reassign(ident, value)
						.map_err(|e| e.with_span(span.clone()))?;
				}
				Instruction {
					kind: InstructionKind::While { cond: expr, body },
					span,
				} => {
					let mut child_scope = Scope::with_parent(scope);

					let cond_lit = match expr.resolve(&child_scope) {
						Ok(cond) => cond,
						Err(e) => return Err(e.with_span(span.clone())),
					};

					let mut cond = match cond_lit {
						Lit::Bool(b) => b,
						other => {
							return Err(error::RuntimeError::InvalidCondition { cond: other }
								.with_span(span.clone()));
						}
					};

					while cond {
						child_scope = Self::_execute(
							out,
							&body.instructions,
							Scope::with_parent(child_scope),
						)?
						.close();

						let cond_lit = match expr.resolve(&child_scope) {
							Ok(cond) => cond,
							Err(e) => return Err(e.with_span(span.clone())),
						};

						cond = match cond_lit {
							Lit::Bool(b) => b,
							other => {
								return Err(error::RuntimeError::InvalidCondition { cond: other }
									.with_span(span.clone()));
							}
						};
					}

					scope = child_scope.close();
				}
			}
		}

		Ok(scope)
	}
}

#[cfg(test)]
mod test {
	use super::*;
	use crate::{
		lexer::{Cmp, Ident, Lit, Op},
		parser::{body::Body, expr::ExprOp, Expr},
		span::Span,
	};

	#[test]
	fn test_execute() {
		let mut executor = Executor::from_iter(vec![
			Instruction::new(InstructionKind::Assign {
				ident: Ident::new("a"),
				value: Expr::Op {
					op: ExprOp::Op(Op::Add),
					lhs: Box::new(Expr::Lit(Lit::Number(1.0))),
					rhs: Box::new(Expr::Lit(Lit::Number(2.0))),
				},
			}),
			Instruction::new(InstructionKind::Print {
				value: Expr::Ident(Ident::new("a")),
			}),
		]);

		let mut out = Vec::new();

		assert!(executor.execute(&mut out).is_ok());
		assert_eq!(out, b"3\n");
	}

	#[test]
	fn test_execute_if() {
		let mut executor = Executor::from_iter(vec![
			Instruction::new(InstructionKind::Assign {
				ident: Ident::new("a"),
				value: Expr::Lit(Lit::Bool(true)),
			}),
			Instruction::new(InstructionKind::If {
				cond: Expr::Ident(Ident::new("a")),
				body: Box::new(Body {
					instructions: vec![Instruction {
						kind: InstructionKind::Print {
							value: Expr::Lit(Lit::String("true".to_string())),
						},
						span: Span::default(),
					}],
				}),
				el: None,
			}),
		]);

		let mut out = Vec::new();

		assert!(executor.execute(&mut out).is_ok());
		assert_eq!(out, b"true\n");
	}

	#[test]
	fn test_execute_if_else() {
		let mut executor = Executor::from_iter(vec![
			Instruction::new(InstructionKind::Assign {
				ident: Ident::new("a"),
				value: Expr::Lit(Lit::Bool(false)),
			}),
			Instruction::new(InstructionKind::If {
				cond: Expr::Ident(Ident::new("a")),
				body: Box::new(Body {
					instructions: vec![Instruction {
						kind: InstructionKind::Print {
							value: Expr::Lit(Lit::String("true".to_string())),
						},
						span: Span::default(),
					}],
				}),
				el: Some(Box::new(Body {
					instructions: vec![Instruction {
						kind: InstructionKind::Print {
							value: Expr::Lit(Lit::String("false".to_string())),
						},
						span: Span::default(),
					}],
				})),
			}),
		]);

		let mut out = Vec::new();

		assert!(executor.execute(&mut out).is_ok());
		assert_eq!(out, b"false\n");
	}

	#[test]
	fn test_execute_while() {
		let mut executor = Executor::from_iter(vec![
			Instruction::new(InstructionKind::Assign {
				ident: Ident::new("a"),
				value: Expr::Lit(Lit::Number(0.0)),
			}),
			Instruction::new(InstructionKind::While {
				cond: Expr::Op {
					op: ExprOp::Cmp(Cmp::Lt),
					lhs: Box::new(Expr::Ident(Ident::new("a"))),
					rhs: Box::new(Expr::Lit(Lit::Number(10.0))),
				},
				body: Box::new(Body {
					instructions: vec![
						Instruction {
							kind: InstructionKind::Reassign {
								ident: Ident::new("a"),
								value: Expr::Op {
									op: ExprOp::Op(Op::Add),
									lhs: Box::new(Expr::Ident(Ident::new("a"))),
									rhs: Box::new(Expr::Lit(Lit::Number(1.0))),
								},
							},
							span: Span::default(),
						},
						Instruction {
							kind: InstructionKind::Print {
								value: Expr::Ident(Ident::new("a")),
							},
							span: Span::default(),
						},
					],
				}),
			}),
			Instruction::new(InstructionKind::Print {
				value: Expr::Ident(Ident::new("a")),
			}),
		]);

		let mut out = Vec::new();

		assert!(executor.execute(&mut out).is_ok());
		assert_eq!(out, b"1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n10\n");
	}
}
