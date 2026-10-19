struct strings : Option<1, 0> {
	struct substr : Option<2, 0> {};
	struct char_access : Option<2, 1> {};
};
struct bit_ops : Option<1, 1> {
	struct bit_and : Option<2, 2> {};
	struct bit_or : Option<2, 3> {};
	struct bit_xor : Option<2, 4> {};
};
