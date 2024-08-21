package site

import "base:runtime"

import "core:mem"
import "core:fmt"

import bilang "../src"

foreign import "env"

foreign env {
	output :: proc "contextless" (string) ---
}

input_buffer: [mem.Megabyte]byte
solve_buffer: [mem.Megabyte]byte

@export get_input_ptr :: proc () -> [^]byte {return raw_data(input_buffer[:])}
@export get_input_len :: proc () -> int     {return len(input_buffer)}

main :: proc () {} // entry point is required to initialize global vars

@export solve :: proc (input_len: int)
{
	solve_arena := mem.Arena{data = solve_buffer[:]}
	context.temp_allocator = mem.arena_allocator(&solve_arena)
	context.allocator = context.temp_allocator

	context.logger = {
		procedure    = proc (
			data_raw: rawptr,
			level:    runtime.Logger_Level,
			text:     string,
			options:  runtime.Logger_Options,
			location: runtime.Source_Code_Location,
		) {
			fmt.print(text)
		},
		data         = {},
		lowest_level = .Warning,
		options      = {},
	}

	input := string(input_buffer[:input_len])

	decls, parse_err := bilang.parse_src(input)

	if parse_err != nil {
		err_str := bilang.parser_error_to_string(input, parse_err)
		output(err_str)
		return
	}

	constrs := bilang.constraints_from_decls(decls)
	bilang.solve(constrs)

	constrs_str, alloc_err := bilang.contraints_to_string(constrs)

	if alloc_err != nil {
		output("Error: failed to allocate memory for output")
		return
	}

	output(constrs_str)
}
