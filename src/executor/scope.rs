use std::{collections::HashMap, fmt, marker::PhantomData};

use crate::{
	error::RuntimeError,
	lexer::{Ident, Lit},
	parser::instruction::r#fn::Fn,
};

#[derive(Debug, Clone)]
pub enum Value {
	Lit(Lit),
	Fn(Fn),
}

impl fmt::Display for Value {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Lit(lit) => write!(f, "{lit}"),
			Self::Fn(fu) => write!(f, "{fu}"),
		}
	}
}

impl From<Lit> for Value {
	fn from(lit: Lit) -> Self {
		Self::Lit(lit)
	}
}

impl From<Fn> for Value {
	fn from(fu: Fn) -> Self {
		Self::Fn(fu)
	}
}

#[derive(Debug, Default)]
pub struct Scope<'a> {
	pub parent: Option<Box<Scope<'a>>>,
	pub vars: HashMap<Ident, Value>,

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

	pub fn get(&self, ident: &Ident) -> Result<&Value, RuntimeError> {
		if let Some(expr) = self.vars.get(ident) {
			return Ok(expr);
		}

		self.parent
			.as_ref()
			.ok_or_else(|| RuntimeError::UnknownVariable(ident.clone()))
			.and_then(|parent| parent.get(ident))
	}

	pub fn set(&mut self, ident: Ident, value: impl Into<Value>) {
		self.vars.insert(ident, value.into());
	}

	pub fn reassign(
		&mut self,
		ident: &Ident,
		value: impl Into<Value>,
	) -> Result<Option<Value>, RuntimeError> {
		if self.vars.contains_key(ident) {
			return Ok(self.vars.insert(ident.clone(), value.into()));
		}

		self.parent
			.as_mut()
			.ok_or_else(|| RuntimeError::UnknownVariable(ident.clone()))
			.and_then(|parent| parent.reassign(ident, value))
	}
}
