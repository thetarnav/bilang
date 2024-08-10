package bilang

import "core:math"

Polynomial :: distinct []f64

@require_results
polynomial_degree :: proc (p: Polynomial) -> int {
	return len(p)-1
}

Polynomial_Error :: union {bool, Allocator_Error}

@require_results
polynomial_from_atom :: proc (
	atom: Atom, max_len: int,
	allocator := context.allocator,
) -> (
	p: Polynomial,
	ok: bool, err: Allocator_Error,
) #no_bounds_check
{
	addends: []Atom
	switch a in atom {
	case Atom_Add:
		addends = a.addends[:]
	case Atom_Mul, Atom_Pow, Atom_Num, Atom_Var:
		addends = {a}
	case Atom_Div:
		return
	}

	buf   := make([]f64,  max_len, allocator) or_return
	p_set := make([]bool, max_len, context.temp_allocator) or_return
	p_len := 0

	for addend in addends {
		i: int
		coefficient: f64
		#partial switch v in addend {
		case Atom_Num:
			i = 0
			coefficient = fraction_float(v)
		case Atom_Var: // TODO: check if vars are the same
			i = 1	
			coefficient = fraction_float(v.f)
		case Atom_Mul:
			if len(v.factors) != 2 {
				return
			}
			num, is_lhs_num  := v.factors[0].(Atom_Num)
			pow, is_rhs_pow  := v.factors[1].(Atom_Pow)
			_  , is_base_var := pow.lhs.(Atom_Var) // TODO: check if vars are the same
			exp, is_exp_num  := pow.rhs.(Atom_Num)
			if !is_lhs_num || !is_rhs_pow || !is_base_var || !is_exp_num {
				return
			}
			exp_f := fraction_float(exp)
			i      = int(exp_f)
			if f64(i) != exp_f {
				return
			}
			coefficient = fraction_float(num)
		case Atom_Pow:
			// only integers allowed,
			// might need to deal with computational round-off later
			// but false negatives are better than false positives here
			_  , is_base_var := v.lhs.(Atom_Var) // TODO: check if vars are the same
			exp, is_exp_num  := v.rhs.(Atom_Num)
			if !is_base_var || !is_exp_num {
				return
			}
			exp_f := fraction_float(exp)
			i      = int(exp_f)
			if f64(i) != exp_f {
				return
			}
			coefficient = 1
		case:
			return
		}

		if i >= max_len || p_set[i] {
			return
		}

		p_set[i] = true
		buf[i]   = coefficient
		p_len    = max(p_len, i+1)
	}

	return Polynomial(buf[:p_len]), true, nil
}

// Returns a slice of derivatives of the same length as the polynomial coefficients
@require_results
polynomial_derivative :: proc (
	p: Polynomial, allocator := context.allocator,
) -> (
	d: Polynomial, err: Allocator_Error,
) #no_bounds_check
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

// Bisection method to find an initial guess
@require_results
bisection :: proc (a, b, tolerance: f64, p: Polynomial) -> (guess: f64) {
	a, b := a, b
	for b-a >= tolerance {
		guess = (a+b) / 2 // Midpoint
		if execute_polynomial(p, guess) == 0.0 {
			break // c is a root
		} else if execute_polynomial(p, guess) * execute_polynomial(p, a) < 0 {
			b = guess
		} else {
			a = guess
		}
	}
	return
}

// Newton-Raphson method
@require_results
newton_raphson :: proc (
	initial_guess, tolerance: f64,
	max_iter: int, p: Polynomial,
) -> (
	x: f64, found_exact: bool,
) {
	d, err := polynomial_derivative(p, context.temp_allocator)
	if err != nil {
		return
	}
	x = initial_guess

	for _ in 0 ..< max_iter {
		fx := execute_polynomial(p, x)
		if math.abs(fx) < tolerance {
			return x, true
		}
		dfx := execute_polynomial(d, x)
		if dfx == 0 {
			return x, false
		}
		x -= fx/dfx
	}
	return x, false
}
