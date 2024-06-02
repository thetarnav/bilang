package bilang

import test "core:testing"


@test test_parser :: proc (t: ^test.T)
{
	input := `
a     = -69.5
a + b = c * 4 + -20
a - b = 10 * (5 + 15) / 2
`

	decls, err := parse_src(input)

	print_decls(decls, false)
}
