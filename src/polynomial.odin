package bilang

import "core:math"
import "core:slice"
import "core:log"

Polynomial :: distinct []f64 // coefficients

MAX_POLYNOMIAL_LEN :: 4

@require_results
polynomial_degree :: proc (p: Polynomial) -> int {
	return len(p)-1
}

@require_results
polynomial_from_atom :: proc (
	atom: Atom, var: string,
	allocator := context.allocator,
) -> (p: Polynomial, ok: bool) #no_bounds_check
{
	@static coeffs: [MAX_POLYNOMIAL_LEN]f64
	@static filled: [MAX_POLYNOMIAL_LEN]bool

	coeffs = 0
	filled = false

	State :: struct {
		var: string,
		len: int,
	}

	s := State{var = var}

	visit_addend(atom, &s) or_return

	cloned, err := slice.clone(coeffs[:s.len], allocator)
	if err != nil {
		log.error("Allocation error when creaging polynomial:", err)
		return
	}

	return Polynomial(cloned), true

	visit_addend :: proc (a: Atom, s: ^State) -> (ok: bool) #no_bounds_check
	{
		idx := MAX_POLYNOMIAL_LEN // out-of-bounds
		coeff: f64

		#partial switch a.kind {
		// a + b
		case .Add:
			return visit_addend(a.lhs^, s) && visit_addend(a.rhs^, s)
		// 123
		case .Num:
			idx, coeff = 0, a.num
		// x
		case .Var:
		 	if a.var == s.var {
				idx, coeff = 1, 1
		 	}
		// x^2
		case .Pow:
			idx = get_index_from_pow(a, s^)
			coeff = 1
		// 2*x^2  or  2*x
		case .Mul:
			b: ^Atom
			switch {
			case a.lhs.kind == .Num: coeff, b = a.lhs.num, a.rhs
			case a.rhs.kind == .Num: coeff, b = a.rhs.num, a.lhs
			case: return
			}

			switch {
			case is_var(b^, s.var): idx = 1
			case b.kind == .Pow   : idx = get_index_from_pow(b^, s^)
			}
		}

		if idx >= MAX_POLYNOMIAL_LEN || filled[idx] {
			return
		}

		filled[idx] = true
		coeffs[idx] = coeff
		s.len       = max(s.len, idx+1)

		return true
	}

	get_index_from_pow :: proc (a: Atom, s: State) -> int {
		if is_var(a.lhs^, s.var) && a.rhs.kind == .Num && is_int(a.rhs.num) {
			return int(a.rhs.num)
		}
		return MAX_POLYNOMIAL_LEN // out-of-bounds
	}
}

// Returns a slice of derivatives of the same length as the polynomial coefficients
@require_results
polynomial_derivative :: proc (
	p: Polynomial, allocator := context.allocator,
) -> (
	d: Polynomial, err: Allocator_Error,
) #no_bounds_check #optional_allocator_error
{
	assert(len(p) > 0)
	buf := make([]f64, len(p)-1, allocator) or_return
	for coefficient, i in p[1:] {
		buf[i] = coefficient * f64(i+1)
	}
	return Polynomial(buf[:len(p)-1]), nil
}

@require_results
execute_polynomial :: proc (p: Polynomial, x: f64) -> (result: f64) {
	if len(p) <= 0 {
		return 0
	}
	result = p[0]
	for pow in 1 ..< len(p) {
		part := x
		for _ in 1 ..< pow {
			part *= x
		}
		part *= p[pow]
		result += part
	}
	return
}

/*
Bisection method to guess root of polynomial
Uses float accuracy instead of tolerance param

`a, b` - range for `x` to try (`a < b`),
`p`    - polynomial `p(x)`,
*/
@require_results
bisection :: proc (a, b: f64, p: Polynomial) -> (x: f64, found: bool) {
	assert(a <= b, "a > b")
	a, b := a, b
	x = math.nan_f64()
	for {
		mid := (a+b) / 2
		if mid == x {
			return x, false
		}
		x = mid
		fx := execute_polynomial(p, x)
		if fx == 0 {
			return x, true
		}
		fa := execute_polynomial(p, a)
		if fx * fa < 0 {
			b = x
		} else {
			a = x
		}
	}
}

/*
Newton-Raphson method to guess root of polynomial
Uses float accuracy instead of tolerance param

`initial_guess` - initial quess for the root,
`p`             - polynomial `p(x)`,
*/
@require_results
newton_raphson :: proc (
	initial_guess: f64, p: Polynomial,
) -> (
	x: f64, found: bool,
) {
	d, err := polynomial_derivative(p, context.temp_allocator)
	if err != nil {
		return
	}

	x = initial_guess
	old_x := x

	for i := 0;; i += 1 {
		fx := execute_polynomial(p, x)
		if fx == 0 {
			return x, true
		}
		dfx := execute_polynomial(d, x)
		if dfx == 0 {
			return x, false
		}
		new_x := x - fx/dfx
		if i > 0 && (new_x == x || new_x == old_x || math.sign(x-old_x) != math.sign(new_x-old_x)) {
			return x, false
		}
		old_x, x = x, new_x
	}
	return x, false
}

// TODO: should give two results
solve_quadratic_polynomial :: proc (a, b, c: f64) -> (root: f64, found: bool)
{
	/* 
	x = (-b +/- (b^2 - 4ac)^(1/2)) / 2a
	*/
	root = (-b + math.sqrt(b*b - 4*a*c)) / (2*a)
	return root, !math.is_nan(root)
}

find_polynomial_root :: proc (p: Polynomial) -> (root: f64, found: bool) #no_bounds_check
{
	if len(p) == 0 {
		return
	}

	if p[0] == 0 {
		return 0, true
	}

	if len(p) == 3 {
		return solve_quadratic_polynomial(p[2], p[1], p[0])
	}

	a, b := -p[0], p[0]
	if a > b {
		a, b = b, a
	}

	root, found = bisection(a, b, p)
	if found do return
	return newton_raphson(root, p)
}
