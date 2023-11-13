use std::{collections::HashMap, marker::PhantomData};

use crate::{
	error::RuntimeError,
	lexer::{Ident, Lit},
};

#[derive(Debug, Default)]
pub struct Scope<'a> {
	pub parent: Option<Box<Scope<'a>>>,
	pub vars: HashMap<Ident, Lit>,

	_phantom: PhantomData<&'a ()>,
}

impl<'a> Scope<'a> {
	pub fn with_parent(parent: Self) -> Scope<'a> {
		Self {
			parent: Some(Box::new(parent)),
			vars: HashMap::new(),
			_phantom: PhantomData,
		}
	}

	pub fn close(self) -> Scope<'a> {
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

	pub fn reassign(&mut self, ident: &Ident, value: Lit) -> Result<Option<Lit>, RuntimeError> {
		if self.vars.contains_key(ident) {
			return Ok(self.vars.insert(ident.clone(), value));
		}

		self.parent
			.as_mut()
			.ok_or_else(|| RuntimeError::UnknownVariable(ident.clone()))
			.and_then(|parent| parent.reassign(ident, value))
	}
}
