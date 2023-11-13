use std::ops::Range;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
	range: Range<usize>,
}

impl Span {
	pub fn new(range: Range<usize>) -> Self {
		Self { range }
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
