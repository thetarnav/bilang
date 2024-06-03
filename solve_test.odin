package bilang

import "core:strings"
import test "core:testing"


@test test_solver :: proc (t: ^test.T)
{
	input :=`
a + b = 10
a     = -4 + 2
`

	expected :=
`b = 12
a = -2
`

	decls, err := parse_src(input)

	constraints := solve(decls)

	b := strings.builder_make_len_cap(0, 1024)
	w := strings.to_writer(&b)

	write_contraints(w, constraints, false)

	output := strings.to_string(b)

	test.expectf(t,
		output == expected,
		"\nEXPECTED:\n%s\nACTUAL:\n%s",
		expected, output,
	)
}
