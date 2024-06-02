package bilang

import "core:strings"
import test "core:testing"


@test test_parser :: proc (t: ^test.T)
{
	input := `
a     = -69.5
a + b = c * 4 + -20
a - b = 10 * (5 + 15) / 2
`

	expected :=
`a = (- 69.5)
(+ a b) = (+ (* c 4) (- 20))
(- a b) = (/ (* 10 (+ 5 15)) 2)
`

	decls, err := parse_src(input)

	b := strings.builder_make_len_cap(0, 1024)
	w := strings.to_writer(&b)

	write_decls(w, decls, false)

	output := strings.to_string(b)

	test.expect_value(t, output, expected)
}
