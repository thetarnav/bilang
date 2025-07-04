package bilang

import "core:testing"
import "core:mem"
import "core:strings"
import "core:log"
import "core:slice"

import "../utils"

@test test_parsing_polynomials :: proc (t: ^testing.T)
{
	@static arena_buf: [mem.Megabyte]byte
	parser_arena: mem.Arena
	mem.arena_init(&parser_arena, arena_buf[:])
	case_allocator := mem.arena_allocator(&parser_arena)

	context.temp_allocator = case_allocator
	context.allocator      = case_allocator

	test_cases := []struct {
		input:      string,
		poly:       Polynomial,
		derivative: Polynomial,
	}{
		// {
		// 	"2*x = 0",
		// 	{0, 2},
		// 	{2},
		// },
		// {
		// 	"2*x + 3 = 0",
		// 	{3, 2},
		// 	{2},
		// },
		// {
		// 	"x^2 - 2*x = 0",
		// 	{0, -2, 1},
		// 	{-2, 2},
		// },
		// {
		// 	"-4*x^3 + 6*x^2 + 2 = 0",
		// 	{2, 0, 6, -4},
		// 	{0, 12, -12},
		// }
	}
	
	for &test_case in test_cases {

		defer free_all(case_allocator)

		decls, parse_err := parse_src(test_case.input)
		if parse_err != nil {
			log.errorf(
				"\nFailed to parse input:\n%s",
				parser_error_to_string(test_case.input, parse_err),
			)
			continue
		}

		constrs := constraints_from_expr(decls)

		x_atom, x_in_constrs := constrs.vars["x"]
		assert(x_in_constrs, "x should be in constraints")

		assert_equal(x_atom.kind,     Atom_Kind.Eq,  "constr atom kind")
		assert_equal(x_atom.rhs.kind, Atom_Kind.Int, "rhs kind")
		assert_equal(x_atom.rhs.int,  0,             "rhs value")

		fold_atom(&x_atom.lhs, &constrs, "x")

		poly, ok := polynomial_from_atom(x_atom.lhs^, "x")
		if !ok {
			log.errorf("\nCouldn't get polynomial for CASE:\n%s", test_case.input)
			return
		}

		testing.expectf(t,
			slice.equal(poly, test_case.poly),
			"\nPolynomial desn't match for CASE:\n%s\n\e[0;32mEXPECTED:\e[0m\n%v\e[0;31m\nACTUAL:\e[0m\n%v",
			test_case.input, test_case.poly, poly,
		)

		derivative := polynomial_derivative(poly)

		testing.expectf(t,
			slice.equal(derivative, test_case.derivative),
			"\nDerivative desn't match for CASE:\n%s\n\e[0;32mEXPECTED:\e[0m\n%v\e[0;31m\nACTUAL:\e[0m\n%v",
			test_case.input, test_case.derivative, derivative,
		)
	}
}

@test test_newton_raphson :: proc (t: ^testing.T)
{
	@static arena_buf: [mem.Megabyte]byte
	parser_arena: mem.Arena
	mem.arena_init(&parser_arena, arena_buf[:])
	case_allocator := mem.arena_allocator(&parser_arena)

	context.temp_allocator = case_allocator
	context.allocator      = case_allocator

	test_cases := []struct {
		poly:  Polynomial,
		root:  f64,
		found: bool,
	}{
		{
			{0, 2}, // 2x
			0,
			true,
		},
		{
			{3, 2}, // 2x + 3
			-1.5,
			true,
		},
		{
			{0, -2, 1}, // x^2 -2x
			0,
			true,
		},
		{
			{-1, -3, 2}, // 2x^2 −3x −1
			1.7807764064044151,
			true,
		},
		{
			{-1, -1, 0, 1}, // x^3 -x -1
			1.324717957244746,
			true,
		},
		{
			{2, 0, 6, -4}, // -4x^3 + 6x^2 + 2
			1.67765069880406,
			false,
		},
		{
			{-5, -2, 0, 2}, // 2x^3 -2x -5
			1.6005985449336209,
			false,
		},
		{
			{8, 12, 1, 1}, // x^3 + x^2 + 12x + 8
			-0.6789995144332207, // ? What to do when the float precisions doesn't allow for perfect answer?
			false,
		},
		// { // This one doesn't cross the y axis
		// 	{520, 12, 1}, // x^2 + 12x + 520
		// 	0.67513140137499406,
		// 	false,
		// },
		{ // Has two solutions: ​x=2√139−6, x=−2√139−6 (17.57965224510319, -29.57965224510319)
			{-520, 12, 1}, // x^2 + 12x - 520
			17.57965224510319,
			true,
		},
	}
	
	for &test_case in test_cases {

		defer free_all(case_allocator)

		root, found := find_polynomial_root(test_case.poly)

		if root != test_case.root || found != test_case.found {
			log.errorf(
				"\nIncorrect results for CASE:\n%v\n\e[0;32mEXPECTED:\e[0m\n%v, %v\e[0;31m\nACTUAL:\e[0m\n%v, %v",
				test_case.poly, test_case.root, test_case.found, root, found,
			)
		}
	}
}
