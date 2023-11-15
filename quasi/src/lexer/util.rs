pub fn is_whitespace(c: &[u8]) -> bool {
	let Some(ch) = c.first() else {
		return true;
	};

	matches!(ch, b' ' | b'\t' | b'\n' | b'\r')
}

pub fn is_gap(c: &[u8]) -> bool {
	let Some(ch) = c.first() else {
		return true;
	};

	is_whitespace(c) || matches!(ch, b'{' | b';')
}
