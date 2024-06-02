package bilang

import "core:fmt"



main :: proc ()
{
	language_input := `
a + b = 10
a     = -4 + 2
`

	decls, err := parse_src(language_input)

	print_decls(decls)

	if err != nil {
		fmt.print(parser_error_to_string(language_input, err))
	}
}
