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

	/// Returns a range that captures the entire span and up to 3 lines before and after,
	/// and the line number (not slice index) of the start and end of the range.
	pub fn line_range(&self, input: &[u8]) -> (usize, usize, Range<usize>) {
		let mut start = self.range.start;
		let mut end = self.range.end;

		// find the start of the line
		while start > 0 && input[start - 1] != b'\n' {
			start -= 1;
		}

		// find the end of the line
		while end < input.len() && input[end] != b'\n' {
			end += 1;
		}

		// find the start of the range
		let mut range_start = start;
		while range_start > 0 && input[range_start - 1] != b'\n' {
			range_start -= 1;
		}

		// find the end of the range
		let mut range_end = end;
		while range_end < input.len() && input[range_end] != b'\n' {
			range_end += 1;
		}

		let line = bytecount::count(&input[..start], b'\n') + 1;
		let line_end = bytecount::count(&input[start..end], b'\n') + line;

		(line, line_end, range_start..range_end)
	}

	pub fn highlight(&self, input: &[u8]) -> String {
		let mut output = String::new();
		let (mut line, line_end, range) = self.line_range(input);
		let number_length = line_end.to_string().len();

		output.push_str(&format!("{line: >number_length$}"));
		output.push_str(" | ");

		for c in &input[range] {
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
