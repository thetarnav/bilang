package bilang

import "core:testing"
import "core:mem"
import "core:strings"
import "core:log"
import "core:slice"

import "../utils"

/*
-4x^3+6x^2+2=0
x ~= 1.67765...
*/

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
		{
			"2*x = 0",
			{0, 2},
			{2},
		},
		{
			"2*x + 3 = 0",
			{3, 2},
			{2},
		},
		{
			"x^2 - 2*x = 0",
			{0, -2, 1},
			{-2, 2},
		},
		{
			"-4*x^3 + 6*x^2 + 2 = 0",
			{2, 0, 6,   -4},
			{0, 12, -12},
		},
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

		constrs := constraints_from_decls(decls)

		utils.assert_equal(len(constrs), 1, "constrs length")
		utils.assert_equal(constrs[0].rhs.(Atom_Num).num, 0, "rhs value")

		_updated: bool
		fold_atom(constrs[0].lhs, &_updated)

		poly, ok, poly_alloc_err := polynomial_from_atom(constrs[0].lhs^, 4)
		if !ok {
			log.errorf("\nCouldn't get polynomial for CASE:\n%s", test_case.input)
			return
		}
		if poly_alloc_err != nil {
			log.errorf("\nAlloc error for CASE:\n%s", test_case.input)
			return
		}

		testing.expectf(t,
			slice.equal(poly, test_case.poly),
			"\nPolynomial desn't match for CASE:\n%s\n\e[0;32mEXPECTED:\e[0m\n%v\e[0;31m\nACTUAL:\e[0m\n%v",
			test_case.input, test_case.poly, poly,
		)

		derivative, derivative_alloc_err := polynomial_derivative(poly)
		if derivative_alloc_err != nil {
			log.errorf("\nAlloc error for CASE:\n%s", test_case.input)
			return
		}

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
		poly:        Polynomial,
		solution:    f64,
		found_exact: bool,
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
			{2, 0, 6, -4}, // -4x^3 + 6x^2 + 2
			1.677650698804577,
			true,
		},
	}
	
	for &test_case in test_cases {

		defer free_all(case_allocator)

		a := -test_case.poly[0]
		b :=  test_case.poly[0]
		initial_guess := bisection(a, b, 1e-6, test_case.poly)
		solution, found_exact := newton_raphson(initial_guess, 1e-6, 10000, test_case.poly)

		if solution != test_case.solution || found_exact != test_case.found_exact {
			log.errorf(
				"\nDifferent results for CASE:\n%v\n\e[0;32mEXPECTED:\e[0m\n%v; %v\e[0;31m\nACTUAL:\e[0m\n%v; %v",
				test_case.poly, test_case.solution, test_case.found_exact, solution, found_exact,
			)
		}
	}
}
