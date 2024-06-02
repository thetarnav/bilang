package bilang

import "core:fmt"


language_input := `
a     = -69.5
a + b = c * 4 + -20
a - b = 10 * (5 + 15) / 2
`

main :: proc ()
{
	decls, err := parse_file(language_input)

	for decl in decls {
		print_assign(decl)
	}

	if err != nil {
		fmt.print(parser_error_to_string(language_input, err))
	}
}
