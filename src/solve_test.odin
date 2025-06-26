package bilang

import "core:strings"
import "core:fmt"
import "core:io"
import "core:mem"
import "core:log"
import test "core:testing"


solve_test_case :: proc(t: ^test.T, input, expected: string, expected_solved := true, expected_good := true) {

	@static arena_buf: [10*mem.Megabyte]byte
	parser_arena: mem.Arena
	mem.arena_init(&parser_arena, arena_buf[:])
	case_allocator := mem.arena_allocator(&parser_arena)
	defer free_all(case_allocator)

	context.temp_allocator = case_allocator
	context.allocator = case_allocator

	expr, err := parse_src(input)
	
	if err != nil {
		log.errorf(
			"\nFailed to parse input:\n%s",
			parser_error_to_string(input, err),
		)
		return
	}

	constrs := constraints_from_expr(expr)
	solved, good := resolve(constrs)

	b := strings.builder_make_len_cap(0, 1024)
	w := strings.to_writer(&b)

	write_constraints(w, constrs)

	output := strings.to_string(b)

	if output != expected || solved != expected_solved || good != expected_good {
		strings.builder_reset(&b) // makes output unusable !!!

		write_constraints(w, constrs, {highlight=true})
		output_pretty := strings.clone(strings.to_string(b))

		strings.builder_reset(&b)

		write_expr(w, expr, {highlight=true})
		io.write_rune(w, '\n')
		exprs_pretty := strings.clone(strings.to_string(b))

		strings.write_string(&b, fmt.aprintf(
			"\n\nCASE:\n%s"+
			"\nPARSED:\n%s",
			input, exprs_pretty,
		))

		if output != expected {
			strings.write_string(&b, fmt.aprintf(
				"\e[0;32mEXPECTED:\e[0m\n%s"+
				"\e[0;31mACTUAL:\e[0m\n%s",
				expected, output_pretty,
			))
		}

		if solved != expected_solved {
			strings.write_string(&b, fmt.aprintf(
				"\nSOLVED: %t \e[0;31m(expected %t)\e[0m",
				solved, expected_solved,
			))
		}

		if good != expected_good {
			strings.write_string(&b, fmt.aprintf(
				", GOOD: %t \e[0;31m(expected %t)\e[0m",
				good, expected_good,
			))
		}

		strings.write_string(&b, "\nTRANSFORMATION HISTORY:\n")
		for var, atom in constrs {
			strings.write_string(&b, var)
			write_highlight(w, .Punct, {highlight=true})
			strings.write_string(&b, ": ")
			write_highlight(w, .Reset, {highlight=true})
			write_atom_transformations(w, atom^, {highlight=true})
			strings.write_string(&b, "\n")
		}

		log.error(strings.to_string(b))
	}
}

@test test_solver :: proc (t: ^test.T)
{
	solve_test_case(t,
		"3 - x + 2 = 3",
		"x = 2\n",
	)

	solve_test_case(t,
		"3 / x = 1",
		"x = 3\n",
	)

	solve_test_case(t,
		"x / 3 = 1",
		"x = 3\n",
	)

	solve_test_case(t,
		"2 * x = 6",
		"x = 3\n",
	)

	solve_test_case(t,
		"6 = x * 2",
		"x = 3\n",
	)

	solve_test_case(t,
		"x = y\n"+
		"y = 1",
		"x = 1\n"+
		"y = 1\n",
	)

	// solve_test_case(t,
	// 	"a + b = 10\n"+
	// 	"a = -4 + 2",
	// 	"a: a = -2\n"+
	// 	"b: b = 12\n",	
	// )

	// solve_test_case(t,
	// 	"y = a * x + b\n"+
	// 	"a = 1 / 2\n"+
	// 	"b = -4\n"+
	// 	"y = 2\n",
	// 	"y: y = 2.0\n"+
	// 	"a: a = 0.5\n"+
	// 	"x: x = 12.0\n"+
	// 	"b: b = -4.0\n"+
	// 	"b: b = -4\n"+
	// 	"y: y = 2\n",
	// 	expected_good=false, // TODO: float == int
	// )

	// solve_test_case(t,
	// 	"x = 1\n"+
	// 	"x = 2",
	// 	"x: x = 1\n"+
	// 	"x: x = 2\n",
	// 	expected_good=false,
	// )

	// solve_test_case(t,
	// 	"(n * 2 + 10) / (n + 1) = 3",
	// 	"n: n = 7\n",
	// )

	// solve_test_case(t,
	// 	"(n * 2 + 10) / (n + 1) = 2 * x\n"+
	// 	"x * 2 = 3\n",
	// 	"n: n = 7.0\n"+
	// 	"x: x = 1.5\n",
	// )

	// solve_test_case(t,
	// 	"(4*n + 10) / (n + 1) = 3*x\n"+
	// 	"2*x = 6",
	// 	"n: n = 0.2\n"+
	// 	"x: x = 3.0\n"+
	// 	"x: x = 3\n",
	// 	expected_good=false, // TODO: float == int
	// )

	// solve_test_case(t,
	// 	"(4*n + 10) / (n + 2) = 2*x + 1/2\n",
	// 	"n: n = (4*x + -9.0) / (3.5 + -2*x)\n"+
	// 	"x: x = (n*-3.5 + -9.0) / (-2*n + -4)\n",
	// 	expected_solved=false,
	// )

	// solve_test_case(t,
	// 	"a*b = 0\n",
	// 	"a: a = 0\n"+
	// 	"b: b = 0\n",
	// 	// TODO: this is not true, it's EITHER a = 0 OR b = 0
	// )

	// solve_test_case(t,
	// 	`a*x + b*y + c = 0`,
	// 	"a: a = (-b*y + -c) / x\n"+
	// 	"x: x = (-b*y + -c) / a\n"+
	// 	"b: b = (-a*x + -c) / y\n"+
	// 	"y: y = (-a*x + -c) / b\n"+
	// 	"c: c = -a*x + -b*y\n",
	// 	expected_solved=false,
	// )

	// solve_test_case(t,
	// 	"2*x + 4*y = 0\n",
	// 	"x: x = -2*y\n"+
	// 	"y: y = -0.5*x\n",
	// 	expected_solved=false,
	// )

	// solve_test_case(t,
	// 	"x * x = 4\n",
	// 	"x: x = 2.0\n",
	// )

	// solve_test_case(t,
	// 	"x * x = 0\n",
	// 	"x: x = 0.0\n",
	// )

	// solve_test_case(t,
	// 	"x = 1/0\n",
	// 	"x: x = Inf\n",
	// )

	// solve_test_case(t,
	// 	"x = -1/0\n",
	// 	"x: x = -Inf\n",
	// )

	// solve_test_case(t,
	// 	"x * x = -1\n",
	// 	"x: x = NaN\n",
	// 	// TODO: fail because x is not real
	// )

	// solve_test_case(t,
	// 	"(x^2 + 12) * (x + 1) = 4",
	// 	"x: x^3 + x^2 + 12*x = -8\n",
	// 	expected_solved=false,
	// )

	// solve_test_case(t,
	// 	"x^3 = 1 + x",
	// 	"x: x = 1.324717957244746\n"
	// )

	// solve_test_case(t,
	// 	"3 * x^2 = 6",
	// 	"x: x = 1.4142135623730951\n",
	// )

	// solve_test_case(t,
	// 	"a = \"hello\" + \"world\"",
	// 	"a: a = \"helloworld\"\n",
	// )

	// solve_test_case(t,
	// 	"a = 2 | 1\n"+
	// 	"b = a * 2",
	// 	"a: a = 2|1\n"+
	// 	"b: b = 4|2\n"+
	// 	"a: a = 2.0|1.0\n",
	// 	expected_good=false, // TODO: float == int
	// )
}
