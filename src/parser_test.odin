package bilang

import "core:strings"
import "core:mem"
import "core:log"
import test "core:testing"


@test test_parser :: proc (t: ^test.T)
{

	cases := []struct{input, solve: string}{
		{
			"a + b * c + d * e = 0",

			"(a + (b * c)) + (d * e) = 0\n",
		},
		{
			"a * b + c * d + e = 0",

			"((a * b) + (c * d)) + e = 0\n",
		},
		{
			"(n * 2 + 10) / (n + 1) = 3",

			"((n * 2) + 10) / (n + 1) = 3\n",
		},
		{
			"\n"+
			"a     = -69.5 + 2"+"\n"+
			"a + b = c * 4 + -20"+"\n"+
			"a - b = 10 * (5 + 15) / 2"+"\n",

			"a = (- 69.5) + 2"+"\n"+
			"a + b = (c * 4) + (- 20)"+"\n"+
			"a - b = (10 * (5 + 15)) / 2"+"\n",
		},
		{
			"a = 1 / 2 / 4\n",

			"a = (1 / 2) / 4\n",
		},
		{
			"a = 1 + 2 / 4 * 6 / 8\n",

			"a = 1 + (((2 / 4) * 6) / 8)\n",
		},
		{
			"a = 0 * 1 + 2^3 * 4 + 5\n",

			"a = ((0 * 1) + ((2 ^ 3) * 4)) + 5\n",
		},
		{
			"a = 2^3^4\n",

			"a = 2 ^ (3 ^ 4)\n",
		},
		{
			"-4*x^3 + 6*x^2 + 2 = 0",

			"(((- 4) * (x ^ 3)) + (6 * (x ^ 2))) + 2 = 0\n",
		}
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
	
		b := strings.builder_make_len_cap(0, 1024)
		w := strings.to_writer(&b)
	
		write_decls(w, decls)
	
		output := strings.to_string(b)

		if output != test_case.solve {

			strings.builder_reset(&b) // makes output unusable !!!

			write_decls(w, decls, {highlight=true})
			output_pretty := strings.clone(strings.to_string(b))

			log.errorf(
				"\n\e[0;32mEXPECTED:\e[0m\n%s\e[0;31mACTUAL:\e[0m\n%s",
				test_case.solve, output_pretty,
			)
		}
	}
}
