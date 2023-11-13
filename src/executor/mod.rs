use crate::{
	error,
	lexer::Lit,
	parser::{instruction::InstructionKind, Instruction},
	scope::Scope,
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
	pub fn execute(&mut self) -> Result<(), Error> {
		Self::_execute(&self.instructions, Scope::with_parent(Scope::default()))?;

		Ok(())
	}

	fn _execute<'a>(
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
						child_scope = Self::_execute(&body.instructions, child_scope)?;
					} else if let Some(el) = el {
						child_scope = Self::_execute(&el.instructions, child_scope)?;
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
					Lit::Number(n) => println!("{}", n),
					Lit::String(s) => println!("{}", s),
					Lit::Bool(b) => println!("{}", b),
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
						child_scope =
							Self::_execute(&body.instructions, Scope::with_parent(child_scope))?
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
