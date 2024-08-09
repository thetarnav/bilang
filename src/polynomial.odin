package bilang

import "core:math"

MAX_POLYNOMIAL_LEN :: 4 // only allow 4 coefficients for now

Polynomial :: struct {
	coefficients: [MAX_POLYNOMIAL_LEN]f64,
	len:  int,
}

@require_results
polynomial_degree :: proc (p: Polynomial) -> int {
	return p.len-1
}

polynomial_from_slice :: proc (coefficients: []f64) -> (p: Polynomial) #no_bounds_check {
	for coefficient, i in coefficients {
		p.coefficients[i] = coefficient
	}
	p.len = len(coefficients)
	return
}

@require_results
polynomial_from_atom :: proc (atom: Atom) -> (p: Polynomial, ok: bool) #no_bounds_check
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

	if len(addends) > MAX_POLYNOMIAL_LEN {
		return
	}

	filled: [MAX_POLYNOMIAL_LEN]bool

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
			if len(v.factors) != 2 do return
			num   := v.factors[0].(Atom_Num) or_return
			pow   := v.factors[1].(Atom_Pow) or_return
			_      = pow.lhs.(Atom_Var) or_return // TODO: check if vars are the same
			exp   := pow.rhs.(Atom_Num) or_return
			exp_f := fraction_float(exp)
			i      = int(exp_f)
			if f64(i) != exp_f do return
			coefficient = fraction_float(num)
		case Atom_Pow:
			// only integers allowed,
			// might need to deal with computational round-off later
			// but false negatives are better than false positives here
			_      = v.lhs.(Atom_Var) or_return // TODO: check if vars are the same
			exp   := v.rhs.(Atom_Num) or_return
			exp_f := fraction_float(exp)
			i      = int(exp_f)
			if f64(i) != exp_f do return
			coefficient = 1
		case:
			return
		}

		if i >= MAX_POLYNOMIAL_LEN || filled[i] {
			return
		}

		filled[i] = true
		p.coefficients[i] = coefficient
		p.len = max(p.len, i+1)
	}

	return p, true
}

// Returns a slice of derivatives of the same length as the polynomial coefficients
@require_results
polynomial_derivatives :: #force_inline proc (p: Polynomial) -> (d: Polynomial) #no_bounds_check {
	d.len = p.len
	for i in 0..<MAX_POLYNOMIAL_LEN {
		d.coefficients[i] = p.coefficients[i] * f64(i)
	}
	return
}

@require_results
execute_polynomial :: proc (p: Polynomial, x: f64) -> (result: f64) {
	result = 1
	for pow in 0 ..< p.len {
		x := x
		for _ in 0 ..< pow {
			x *= x
		}
		x *= p.coefficients[pow]
		result += x
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
	derivatives := polynomial_derivatives(p)
	x = initial_guess
	
	for _ in 0 ..< max_iter {
		fx := execute_polynomial(p, x)
		if math.abs(fx) < tolerance {
			return x, true
		}
		dfx := execute_polynomial(derivatives, x)
		if dfx == 0 {
			return x, false
		}
		x -= fx/dfx
	}
	return x, false
}
