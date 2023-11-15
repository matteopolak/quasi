use std::ops::Range;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Span {
	range: Range<usize>,
}

impl From<Range<usize>> for Span {
	fn from(range: Range<usize>) -> Self {
		Self::new(range)
	}
}

impl Span {
	pub const fn new(range: Range<usize>) -> Self {
		Self { range }
	}

	pub fn len(&self) -> usize {
		self.range.len()
	}

	pub fn is_empty(&self) -> bool {
		self.range.is_empty()
	}

	pub fn merge(&self, other: &Self) -> Self {
		Self {
			range: self.range.start.min(other.range.start)..self.range.end.max(other.range.end),
		}
	}

	pub fn line_start(&self, input: &[u8]) -> usize {
		bytecount::count(&input[..self.range.start], b'\n') + 1
	}

	pub fn line_end(&self, input: &[u8]) -> usize {
		bytecount::count(&input[..self.range.end], b'\n') + 1
	}

	/// Creates a new span that extends the current one by `lines` lines
	/// in both directions.
	pub fn expand(&self, input: &[u8], lines: usize) -> Self {
		if input.is_empty() {
			return self.clone();
		}

		let mut start = self.range.start;
		let mut end = self.range.end;

		for _ in 0..lines {
			while start > 0 && input[start - 1] != b'\n' {
				start -= 1;
			}

			start = start.saturating_sub(1);
		}

		for _ in 0..lines {
			while end < input.len() && input[end] != b'\n' {
				end += 1;
			}

			if end < input.len() {
				end += 1;
			}
		}

		Self { range: start..end }
	}

	pub fn highlight(&self, input: &[u8]) -> String {
		let mut output = String::new();
		let lines = self.expand(input, 2);
		let (mut line, line_end) = (lines.line_start(input), lines.line_end(input));

		let number_length = line_end.to_string().len();

		output.push_str(&format!("{line: >number_length$}"));
		output.push_str(" | ");

		for c in &input[lines.range] {
			if *c == b'\n' {
				line += 1;

				output.push('\n');
				output.push_str(&format!("{line: >number_length$}"));
				output.push_str(" | ");

				continue;
			}

			output.push(*c as char);
		}

		output
	}

	pub fn range(&self) -> &Range<usize> {
		&self.range
	}

	pub fn offset(mut self, offset: usize) -> Self {
		self.range.start += offset;
		self.range.end += offset;
		self
	}

	pub fn overlaps(&self, other: &Self) -> bool {
		// checks if self overlaps with other, either partially or fully
		self.range.start <= other.range.end && self.range.end >= other.range.start
	}

	pub fn to(&self, other: &Self) -> Self {
		Self {
			range: self.range.start..other.range.end,
		}
	}
}
