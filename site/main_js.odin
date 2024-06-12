package site

import "core:fmt"

import bilang "../src"


main :: proc () {
	language_input := `
a + b = 10
a     = -4 + 2
`

	// parser_scratch: mem.Scratch_Allocator
	// mem.scratch_allocator_init(&parser_scratch, 1024)

	decls, err := bilang.parse_src(language_input)

	if err != nil {
		fmt.eprint(bilang.parser_error_to_string(language_input, err, context.temp_allocator))
		return
	}

	// bilang.print_decls(decls)

	constrs := bilang.solve(decls)

	fmt.print("\n-------\n\n")
	// bilang.print_contraints(constrs)

	// mem.scratch_allocator_destroy(&parser_scratch)
}
