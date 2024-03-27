mod resolve;

pub mod scope;
use std::io::Write;

pub use scope::Scope;

use crate::{
	error,
	lexer::Lit,
	parser::{
		instruction::{
			assign::Assign, r#if::If, r#while::While, reassign::Reassign, InstructionKind,
		},
		Instruction,
	},
	Error,
};

pub struct Executor {
	pub instructions: Vec<Instruction>,
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

	#[allow(clippy::too_many_lines)]
	pub fn _execute(
		out: &mut impl Write,
		instructions: &[Instruction],
		mut scope: Scope,
	) -> Result<Scope, Error> {
		for instruction in instructions {
			match instruction {
				Instruction {
					kind: InstructionKind::Return(expr),
					span,
				} => {
					let (scope, value) = expr.resolve(scope, out, span)?;

					return Err(
						error::RuntimeError::Return { value, scope }.with_span(span.clone())
					);
				}
				Instruction {
					kind: InstructionKind::Assign(Assign { ident, value }),
					span,
				} => {
					let (s, value) = value.resolve(scope, out, span)?;

					scope = s;
					scope.set(ident.clone(), value);
				}
				Instruction {
					kind: InstructionKind::If(If { cond, body, el }),
					span,
				} => {
					let mut child_scope = Scope::with_parent(scope);

					let (s, cond) = cond.resolve(child_scope, out, span).and_then(|(s, l)| {
						l.try_bool()
							.map_err(|e| e.with_span(span.clone()))
							.map(|b| (s, b))
					})?;

					child_scope = s;

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
				} => {
					let (s, lit) = value.resolve(scope, out, span)?;

					scope = s;

					match lit {
						Lit::Number(n) => writeln!(out, "{n}")?,
						Lit::String(s) => writeln!(out, "{s}")?,
						Lit::Bool(b) => writeln!(out, "{b}")?,
					}
				}
				Instruction {
					kind: InstructionKind::Reassign(Reassign { ident, value }),
					span,
				} => {
					let (s, value) = value.resolve(scope, out, span)?;

					scope = s;
					scope
						.reassign(ident, value)
						.map_err(|e| e.with_span(span.clone()))?;
				}
				Instruction {
					kind: InstructionKind::While(While { cond: expr, body }),
					span,
				} => {
					let mut child_scope = Scope::with_parent(scope);

					let (s, mut cond) =
						expr.resolve(child_scope, out, span).and_then(|(s, l)| {
							l.try_bool()
								.map_err(|e| e.with_span(span.clone()))
								.map(|b| (s, b))
						})?;

					child_scope = s;

					while cond {
						child_scope = Self::_execute(
							out,
							&body.instructions,
							Scope::with_parent(child_scope),
						)?
						.close();

						let (s, c) = expr.resolve(child_scope, out, span).and_then(|(s, l)| {
							l.try_bool()
								.map_err(|e| e.with_span(span.clone()))
								.map(|b| (s, b))
						})?;

						cond = c;
						child_scope = s;
					}

					scope = child_scope.close();
				}
				Instruction {
					kind: InstructionKind::FnCall(call),
					span,
				} => {
					let mut child_scope = Scope::with_parent(scope);

					child_scope = match call.execute(child_scope, out, span) {
						Err(Error::Runtime {
							source: error::RuntimeError::Return { scope, .. },
							..
						}) => scope,
						o => o?,
					};
					scope = child_scope.close();
				}
				Instruction {
					kind: InstructionKind::Fn(fu),
					..
				} => {
					let value = fu.clone();

					scope.set_fn(fu.name.clone(), value);
				}
				Instruction {
					kind: InstructionKind::For(r#for),
					span,
				} => {
					let mut child_scope = Scope::with_parent(scope);

					child_scope = Self::_execute(out, &[*r#for.setup.clone()], child_scope)?;

					let (s, mut cond) =
						r#for
							.cond
							.resolve(child_scope, out, span)
							.and_then(|(s, l)| {
								l.try_bool()
									.map_err(|e| e.with_span(span.clone()))
									.map(|b| (s, b))
							})?;

					child_scope = s;

					while cond {
						let mut inner_scope = Scope::with_parent(child_scope);

						inner_scope = Self::_execute(out, &r#for.body.instructions, inner_scope)?;
						child_scope = inner_scope.close();
						child_scope = Self::_execute(out, &[*r#for.update.clone()], child_scope)?;

						let (s, c) =
							r#for
								.cond
								.resolve(child_scope, out, span)
								.and_then(|(s, l)| {
									l.try_bool()
										.map_err(|e| e.with_span(span.clone()))
										.map(|b| (s, b))
								})?;

						cond = c;
						child_scope = s;
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
		lexer::{Cmp, Ident, Op},
		parser::{body::Body, expr::ExprOp, Expr},
		span::Span,
	};

	#[test]
	fn test_execute() {
		let mut executor = Executor::from_iter(vec![
			Instruction::new(InstructionKind::Assign(Assign {
				ident: Ident::new("a"),
				value: Expr::Op {
					op: ExprOp::Op(Op::Add),
					lhs: Box::new(Expr::Lit(Lit::Number(1.0))),
					rhs: Box::new(Expr::Lit(Lit::Number(2.0))),
				},
			})),
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
			Instruction::new(InstructionKind::Assign(Assign {
				ident: Ident::new("a"),
				value: Expr::Lit(Lit::Bool(true)),
			})),
			Instruction::new(InstructionKind::If(If {
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
			})),
		]);

		let mut out = Vec::new();

		assert!(executor.execute(&mut out).is_ok());
		assert_eq!(out, b"true\n");
	}

	#[test]
	fn test_execute_if_else() {
		let mut executor = Executor::from_iter(vec![
			Instruction::new(InstructionKind::Assign(Assign {
				ident: Ident::new("a"),
				value: Expr::Lit(Lit::Bool(false)),
			})),
			Instruction::new(InstructionKind::If(If {
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
			})),
		]);

		let mut out = Vec::new();

		assert!(executor.execute(&mut out).is_ok());
		assert_eq!(out, b"false\n");
	}

	#[test]
	fn test_execute_while() {
		let mut executor = Executor::from_iter(vec![
			Instruction::new(InstructionKind::Assign(Assign {
				ident: Ident::new("a"),
				value: Expr::Lit(Lit::Number(0.0)),
			})),
			Instruction::new(InstructionKind::While(While {
				cond: Expr::Op {
					op: ExprOp::Cmp(Cmp::Lt),
					lhs: Box::new(Expr::Ident(Ident::new("a"))),
					rhs: Box::new(Expr::Lit(Lit::Number(10.0))),
				},
				body: Box::new(Body {
					instructions: vec![
						Instruction {
							kind: InstructionKind::Reassign(Reassign {
								ident: Ident::new("a"),
								value: Expr::Op {
									op: ExprOp::Op(Op::Add),
									lhs: Box::new(Expr::Ident(Ident::new("a"))),
									rhs: Box::new(Expr::Lit(Lit::Number(1.0))),
								},
							}),
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
			})),
			Instruction::new(InstructionKind::Print {
				value: Expr::Ident(Ident::new("a")),
			}),
		]);

		let mut out = Vec::new();

		assert!(executor.execute(&mut out).is_ok());
		assert_eq!(out, b"1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n10\n");
	}
}
