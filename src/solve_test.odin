package bilang

import "core:strings"
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
		// {
		// 	"3 / x = 1",

		// 	"x: x = 3\n",
		// },
		// {
		// 	"x / 3 = 1",

		// 	"x: x = 3\n",
		// },
		// {
		// 	"2 * x = 6",

		// 	"x: x = 3\n",
		// },
		// {
		// 	"6 = x * 2",

		// 	"x: x = 3\n",
		// },
		// {
		// 	"a + b = 10\n"+
		// 	"a = -4 + 2",

		// 	"a: a = -2\n"+
		// 	"b: b = 12\n"+
		// 	"a: a = -2\n",	
		// },
		// {
		// 	"y = a * x + b\n"+
		// 	"a = 1 / 2\n"+
		// 	"b = -4\n"+
		// 	"y = 2\n",

		// 	"y: y = 2\n"+
		// 	"a: a = 0.5\n"+
		// 	"x: x = 12\n"+
		// 	"b: b = -4\n"+
		// 	"a: a = 0.5\n"+
		// 	"b: b = -4\n"+
		// 	"y: y = 2\n",
		// },
		// {
		// 	"(n * 2 + 10) / (n + 1) = 3",

		// 	"n: n = 7\n",
		// },
		// {
		// 	"(n * 2 + 10) / (n + 1) = 2 * x\n"+
		// 	"x * 2 = 3\n",

		// 	"n: n = 7\n"+
		// 	"x: x = 1.5\n"+
		// 	"x: x = 1.5\n",
		// },
		// {
		// 	"(4*n + 10) / (n + 1) = 2*x + 1/2\n",

		// 	"n: n = (-9.5 + 2*x) / (3.5 + -2*x)\n"+
		// 	"x: x = (19 + 7*n) / (4*n + 4)\n",
		// },
		// {
		// 	"a*b = 0\n",

		// 	"a: a = 0\n"+
		// 	"b: b = 0\n",
		// 	// TODO: this is not true, it's EITHER a = 0 OR b = 0
		// },
		// {
		// 	"a/b = 0\n",

		// 	"a: a = 0\n"+
		// 	"b: 0 = 0\n",
		// 	// TODO: b != 0
		// },
		// {
		// 	"a*x + b*y + c = 0\n",

		// 	"a: a = (-c + (-b * y)) / x\n"+
		// 	"x: x = (-c + (-b * y)) / a\n"+
		// 	"b: b = (-c + (-a * x)) / y\n"+
		// 	"y: y = (-c + (-a * x)) / b\n"+
		// 	"c: c = (-a * x) + (-b * y)\n",
		// },
		// {
		// 	"2*x + 4*y = 0\n",

		// 	"x: x = -2*y\n"+
		// 	"y: y = -0.5*x\n"
		// },
		// {
		// 	"x * x = 4\n",

		// 	"x: x = 2\n",
		// },
		// {
		// 	"x * x = -1\n",

		// 	"x: x = NaN\n",
		// 	// TODO: fail because x is not real
		// },
		// {
		// 	"(x^2 + 12) * (x + 1) = 4",

		// 	"x: x^3 + x^2 + 12*x = -8\n",
		// },
		// {
		// 	"x^3 = 1 + x",

		// 	"x: x = 1.324717957244746\n"
		// }
		// {
		// 	"a = a + 1\n",

		// 	"a: 0 = 1\n",
		// }
	}

	@static arena_buf: [10*mem.Megabyte]byte
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

		constrs := constraints_from_decls(decls)
		solve(constrs)
	
		b := strings.builder_make_len_cap(0, 1024)
		w := strings.to_writer(&b)
	
		write_contraints(w, constrs)
	
		output := strings.to_string(b)

		if output != test_case.solve {

			strings.builder_reset(&b) // makes output unusable !!!

			write_contraints(w, constrs, {highlight=true})
			output_pretty := strings.clone(strings.to_string(b))

			strings.builder_reset(&b)

			write_decls(w, decls, {highlight=true})
			decls_pretty := strings.clone(strings.to_string(b))

			log.errorf(
				"\n\nCASE:\n%s\nPARSED:\n%s\e[0;32mEXPECTED:\e[0m\n%s\e[0;31mACTUAL:\e[0m\n%s",
				test_case.input, decls_pretty, test_case.solve, output_pretty,
			)
		}
	}
}
