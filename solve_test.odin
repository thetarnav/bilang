package bilang

import "core:strings"
import "core:fmt"
import "core:mem"
import "core:log"
import test "core:testing"


@test test_solver :: proc (t: ^test.T)
{
	cases := []struct {
		input: string,
		solve: string,
	}{
		{
			"3 - x + 2 = 3",

			"x: x = 2\n",
		},
		{
			"3 / x = 1",

			"x: x = 3\n",
		},
		{
			"x / 3 = 1",

			"x: x = 3\n",
		},
		{
			"2 * x = 6",

			"x: x = 3\n",
		},
		{
			"6 = x * 2",

			"x: x = 3\n",
		},
		{
			"a + b = 10\n"+
			"a = -4 + 2",

			"a: a = -2\n"+
			"b: b = 12\n"+
			"a: a = -2\n",	
		},
		{
			"y = a * x + b\n"+
			"a = 1 / 2\n"+
			"b = -4\n"+
			"y = 2\n",

			"y: y = 2\n"+
			"a: a = 0.5\n"+
			"x: x = 12\n"+
			"b: b = -4\n"+
			"a: a = 0.5\n"+
			"b: b = -4\n"+
			"y: y = 2\n",
		},
		{
			"(n * 2 + 10) / (n + 1) = 3",

			"n: n = 7\n",
		},
		{
			"(n * 2 + 10) / (n + 1) = 2 * x\n"+
			"x * 2 = 3\n",

			"n: n = 7\n"+
			"x: x = 1.5\n"+
			"x: x = 1.5\n",
		},
		{
			"(4*n + 10) / (n + 1) = 2*x + 1/2\n",

			"n: n = (/ (+ -9.5 2x) (+ 3.5 -2x))\n"+
			"x: x = (/ (+ 7n 19) (+ 4n 4))\n",
		},
	}

	@static arena_buf: [mem.Megabyte]byte
	parser_arena: mem.Arena
	mem.arena_init(&parser_arena, arena_buf[:])
	case_allocator := mem.arena_allocator(&parser_arena)

	context.temp_allocator = case_allocator
	context.allocator = case_allocator
	
	for test_case in cases {

		defer free_all(case_allocator)


		decls, err := parse_src(test_case.input)
		
		if err != nil {
			log.errorf(
				"\nFailed to parse input:\n%s",
				parser_error_to_string(test_case.input, err),
			)
			continue
		}
	
		constrs := solve(decls)
	
		b := strings.builder_make_len_cap(0, 1024)
		w := strings.to_writer(&b)
	
		write_contraints(w, constrs, false)
	
		output := strings.to_string(b)

		if output != test_case.solve {

			strings.builder_reset(&b) // makes output unusable !!!

			write_contraints(w, constrs)
			output_pretty := strings.clone(strings.to_string(b))

			strings.builder_reset(&b)

			write_decls(w, decls)
			decls_pretty := strings.clone(strings.to_string(b))

			log.errorf(
				"\n\nCASE:\n%s\nPARSED:\n%s\e[0;32mEXPECTED:\e[0m\n%s\e[0;31mACTUAL:\e[0m\n%s",
				test_case.input, decls_pretty, test_case.solve, output_pretty,
			)
		}
	
	}
}
