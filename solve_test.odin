package bilang

import "core:strings"
import "core:fmt"
import "core:mem"
import test "core:testing"


@test test_solver :: proc (t: ^test.T)
{
	cases := []struct {
		input   : string,
		expected: string,
	}{
		{
			"3 - x + 2 = 3",

			"x = 2"+"\n",
		},
		{
			"3 / x = 1",

			"x = 3"+"\n",
		},
		{
			"x / 3 = 1",

			"x = 3"+"\n",
		},
		{
			"2 * x = 6",

			"x = 3"+"\n",
		},
		{
			"6 = x * 2",

			"x = 3"+"\n",
		},
		{
			"a + b = 10"+"\n"+
			"a = -4 + 2",

			"b = 12"+"\n"+
			"a = -2"+"\n",	
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
			test.errorf(t,
				"\nFailed to parse input:\n%s",
				parser_error_to_string(test_case.input, err),
			)
			continue
		}
	
		constraints := solve(decls)
	
		b := strings.builder_make_len_cap(0, 1024)
		w := strings.to_writer(&b)
	
		write_contraints(w, constraints, false)
	
		output := strings.to_string(b)
	
		test.expectf(t,
			output == test_case.expected,
			"\n\nCASE:\n%s\n\e[0;32mEXPECTED:\e[0m\n%s\e[0;31mACTUAL:\e[0m\n%s",
			test_case.input, test_case.expected, output,
		)
	}
}