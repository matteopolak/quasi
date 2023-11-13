use std::{fmt, io::Write};

use crate::{
	error::ParseError,
	executor::{Executor, Scope},
	expect,
	lexer::{Delim, Ident, Symbol, Token, TokenKind},
	parser::{body::Body, Expr, Parse, TokenStream},
	span::Span,
	Error,
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Fn {
	pub name: Ident,
	pub args: Vec<Ident>,
	pub body: Box<Body>,
}

impl fmt::Display for Fn {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "fn {} ", self.name)?;

		for (i, arg) in self.args.iter().enumerate() {
			if i != 0 {
				write!(f, ", ")?;
			}

			write!(f, "{arg}")?;
		}

		write!(f, "{{ <body> }}")
	}
}

impl Parse for Fn {
	fn parse(tokens: &mut TokenStream) -> Result<Self, ParseError>
	where
		Self: Sized,
	{
		expect!(tokens, [Keyword(Symbol::Fn) => Keyword(Symbol::Fn)]);

		let name = match tokens.next().map(|t| t.kind) {
			Some(TokenKind::Ident(ident)) => ident,
			other => {
				return Err(ParseError::expected(
					vec![TokenKind::Ident(Ident::default())],
					other,
				))
			}
		};

		expect!(tokens, [OpenDelim(Delim::Paren) => OpenDelim(Delim::Paren)]);

		let mut args = Vec::new();

		while let Some(TokenKind::Ident(ident)) = tokens.peek().map(Token::kind) {
			args.push(ident.clone());

			tokens.next();

			if let Some(TokenKind::Comma) = tokens.peek().map(Token::kind) {
				tokens.next();
			} else {
				break;
			}
		}

		expect!(tokens, [CloseDelim(Delim::Paren) => CloseDelim(Delim::Paren)]);

		let body = Body::parse(tokens)?;

		Ok(Self {
			name,
			args,
			body: Box::new(body),
		})
	}
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FnCall {
	pub name: Ident,
	pub args: Vec<Expr>,
}

impl FnCall {
	pub fn execute(&self, scope: Scope, out: &mut impl Write, span: &Span) -> Result<Scope, Error> {
		let function = scope
			.get_fn(&self.name)
			.cloned()
			.map_err(|e| e.with_span(span.clone()))?;

		let mut fn_scope = Scope::with_parent(scope);

		for (arg, param) in self.args.iter().zip(function.args.iter()) {
			let (scope, value) = arg.resolve(fn_scope, out, span)?;

			fn_scope = scope;
			fn_scope.set(param.clone(), value);
		}

		fn_scope = Executor::_execute(out, &function.body.instructions, fn_scope)?;

		Ok(fn_scope.close())
	}
}

impl Parse for FnCall {
	fn parse(tokens: &mut TokenStream) -> Result<Self, ParseError>
	where
		Self: Sized,
	{
		let name = match tokens.next().map(|t| t.kind) {
			Some(TokenKind::Ident(ident)) => ident,
			other => {
				return Err(ParseError::expected(
					vec![TokenKind::Ident(Ident::default())],
					other,
				))
			}
		};

		expect!(tokens, [OpenDelim(Delim::Paren) => OpenDelim(Delim::Paren)]);

		let mut args = Vec::new();

		loop {
			if let Some(TokenKind::CloseDelim(Delim::Paren)) = tokens.peek().map(Token::kind) {
				break;
			}

			args.push(Expr::parse(tokens)?);

			if let Some(TokenKind::Comma) = tokens.peek().map(Token::kind) {
				tokens.next();
			} else {
				break;
			}
		}

		expect!(tokens, [CloseDelim(Delim::Paren) => CloseDelim(Delim::Paren)]);

		Ok(Self { name, args })
	}
}
