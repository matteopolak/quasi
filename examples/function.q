fn add_and_print(a, b) [
	print a + b;
]

fn is_even(a) [
	if a % 2 == 0 [
		print "even";
	] else [
		print "odd";
	]
]

add_and_print(1, 2); # 3
is_even(10); # even
is_even(15); # odd
