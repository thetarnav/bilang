package site

import "core:fmt"
import "core:mem"

import bilang "../src"

foreign import "env"

foreign env {
	output :: proc "contextless" (string) ---
}

input_buffer: [mem.Megabyte]byte
solve_buffer: [mem.Megabyte]byte

@export get_input_ptr :: proc () -> [^]byte {return raw_data(input_buffer[:])}
@export get_input_len :: proc () -> int     {return len(input_buffer)}

@export solve :: proc (input_len: int)
{
	solve_arena := mem.Arena{data = solve_buffer[:]}
	context.temp_allocator = mem.arena_allocator(&solve_arena)

	input := string(input_buffer[:input_len])

	fmt.println("input: ", input)

	decls, parse_err := bilang.parse_src(input, context.temp_allocator)

	if parse_err != nil {
		err_str := bilang.parser_error_to_string(input, parse_err, context.temp_allocator)
		output(err_str)
		return
	}

	fmt.println("parsed", decls)

	constrs := bilang.solve(decls, context.temp_allocator)

	fmt.println("solved", constrs)

	constrs_str, alloc_err := bilang.contraints_to_string(constrs, highlight=false, allocator=context.temp_allocator)

	if alloc_err != nil {
		output("Error: failed to allocate memory for output")
		return
	}

	fmt.println("output: ", constrs_str)

	output(constrs_str)
}
