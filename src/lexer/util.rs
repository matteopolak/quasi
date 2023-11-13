pub fn is_whitespace(c: u8) -> bool {
	matches!(c, b' ' | b'\t' | b'\n' | b'\r')
}

pub fn is_gap(c: u8) -> bool {
	is_whitespace(c) || matches!(c, b'{')
}
