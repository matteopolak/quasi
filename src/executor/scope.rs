use std::collections::HashMap;

use crate::{
	error::RuntimeError,
	lexer::{Ident, Lit},
	parser::instruction::r#fn::Fn,
};

#[derive(Debug, Default)]
pub struct Scope {
	pub parent: Option<Box<Scope>>,
	pub vars: HashMap<Ident, Lit>,
	pub fns: HashMap<Ident, Fn>,
}

impl Scope {
	pub fn with_parent(parent: Self) -> Scope {
		Self {
			parent: Some(Box::new(parent)),
			..Default::default()
		}
	}

	pub fn close(self) -> Scope {
		// SAFETY: Every scope has a parent, even the root scope.
		unsafe { *self.parent.unwrap_unchecked() }
	}

	pub fn get(&self, ident: &Ident) -> Result<&Lit, RuntimeError> {
		if let Some(expr) = self.vars.get(ident) {
			return Ok(expr);
		}

		self.parent
			.as_ref()
			.ok_or_else(|| RuntimeError::UnknownVariable(ident.clone()))
			.and_then(|parent| parent.get(ident))
	}

	pub fn set(&mut self, ident: Ident, value: Lit) {
		self.vars.insert(ident, value);
	}

	/// Reassigns a variable and returns the old value.
	pub fn reassign(&mut self, ident: &Ident, value: Lit) -> Result<Option<Lit>, RuntimeError> {
		if self.vars.contains_key(ident) {
			return Ok(self.vars.insert(ident.clone(), value));
		}

		self.parent
			.as_mut()
			.ok_or_else(|| RuntimeError::UnknownVariable(ident.clone()))
			.and_then(|parent| parent.reassign(ident, value))
	}

	pub fn get_fn(&self, ident: &Ident) -> Result<&Fn, RuntimeError> {
		if let Some(expr) = self.fns.get(ident) {
			return Ok(expr);
		}

		self.parent
			.as_ref()
			.ok_or_else(|| RuntimeError::UnknownVariable(ident.clone()))
			.and_then(|parent| parent.get_fn(ident))
	}

	pub fn set_fn(&mut self, ident: Ident, value: Fn) {
		self.fns.insert(ident, value);
	}
}
