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
		input: string,
		poly:  []f64,
	}{
		{
			"2*x = 0",
			{0, 2},
		},
		{
			"2*x + 3 = 0",
			{3, 2},
		},
		{
			"x^2 - 2*x = 0",
			{0, -2, 1},
		},
		{
			"-4*x^3 + 6*x^2 + 2 = 0",
			{2, 0, 6, -4},
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

		poly_expected := polynomial_from_slice(test_case.poly)
		poly_actual, ok := polynomial_from_atom(constrs[0].lhs^)

		if ok {
			testing.expectf(t,
				slice.equal(poly_actual.coefficients[:poly_actual.len], poly_expected.coefficients[:poly_expected.len]),
				"\nCASE:\n%s\n\e[0;32mEXPECTED:\e[0m\n%v\e[0;31mACTUAL:\e[0m\n%v",
				test_case.input, poly_expected, poly_actual,
			)
		} else {
			log.errorf("\nCouldn't get polynomial for CASE:\n%s", test_case.input)
		}
	}
}
