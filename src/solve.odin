package bilang

import "base:intrinsics"

import "core:math"
import "core:slice"
import "core:strings"

import "../utils"

/*
atom pointers can be repeated, (same pointers in multiple places)
so the value of atom behind a pointer must stay the same,
when changing the value a new atom must be allocated and pointer changed
*/
Atom :: struct {
	kind: Atom_Kind,
	from: ^Atom, // previous atom from which this one was created
	using _: struct #raw_union {
		using bin: struct {lhs, rhs: ^Atom},
		float: f64,
		int: int,
		str: string,
		var: string,
	},
}

Atom_Kind :: enum u8 {
	Int,
	Float,
	Str,
	Var,
	Add,
	Mul,
	Div,
	Pow,
	Or,
	Eq,
}

ATOM_NUM_KINDS    :: bit_set[Atom_Kind]{.Int, .Float}
ATOM_BINARY_KINDS :: bit_set[Atom_Kind]{.Add, .Div, .Mul, .Pow, .Or, .Eq}

Constraint :: struct {
	var: string, atom: ^Atom,
}

@require_results
atom_new :: proc (atom: Atom, from: ^Atom = nil, loc := #caller_location) -> ^Atom {
	atom := atom
	atom.from = from
	a, err := new_val(Atom, atom, loc=loc)
	// TODO: setup a separate allocator for errors
	utils.alloc_error_assert("atom_new error: ", err, loc)
	return a
}

atom_num_zero    := Atom{kind=.Int, int= 0}
atom_num_one     := Atom{kind=.Int, int= 1}
atom_num_neg_one := Atom{kind=.Int, int=-1}

@require_results
atom_int :: proc (val: int, from: ^Atom = nil, loc := #caller_location) -> ^Atom {
	switch val {
	case  0: return from == nil ? &atom_num_zero : atom_new(atom_num_zero, from, loc)
	case  1: return from == nil ? &atom_num_one : atom_new(atom_num_one, from, loc)
	case -1: return from == nil ? &atom_num_neg_one : atom_new(atom_num_neg_one, from, loc)
	case:    return atom_new({kind=.Int, int=val}, from, loc)
	}
}

@require_results
atom_float :: proc (val: f64, from: ^Atom = nil, loc := #caller_location) -> ^Atom {
	return atom_new({kind=.Float, float=val}, from, loc)
}

@require_results
atom_str :: proc (val: string, from: ^Atom = nil, loc := #caller_location) -> ^Atom {
	return atom_new({kind=.Str, var=val}, from, loc)
}

@require_results
atom_var :: proc (var: string, from: ^Atom = nil, loc := #caller_location) -> ^Atom {
	return atom_new({kind=.Var, var=var}, from, loc)
}

@require_results
atom_bin :: proc (kind: Atom_Kind, lhs, rhs: ^Atom, from: ^Atom = nil, loc := #caller_location) -> ^Atom {
	return atom_new({kind=kind, lhs=lhs, rhs=rhs}, from, loc)
}

atom_num :: proc {atom_int, atom_float}

@require_results atom_is_binary :: proc (atom: Atom) -> bool {return atom.kind in ATOM_BINARY_KINDS}
@require_results atom_is_num    :: proc (atom: Atom) -> bool {return atom.kind in ATOM_NUM_KINDS}

@require_results
atom_val_var_equals :: proc (atom: Atom, var: string) -> bool {
	return atom.kind == .Var && atom.var == var
}
@require_results
atom_val_int_equals :: proc (atom: Atom, val: int) -> bool {
	return atom.kind == .Int && atom.int == val
}
@require_results
atom_val_float_equals :: proc (atom: Atom, val: f64) -> bool {
	return atom.kind == .Float && atom.float == val
}
atom_val_equals :: proc{atom_val_var_equals, atom_val_int_equals, atom_val_float_equals}

atom_num_equals_zero :: proc (atom: Atom) -> bool {
	#partial switch atom.kind {
	case .Int:   return atom.int == 0
	case .Float: return atom.float == 0
	}
	return false
}
atom_num_equals_one :: proc (atom: Atom) -> bool {
	#partial switch atom.kind {
	case .Int:   return atom.int == 1
	case .Float: return atom.float == 1
	}
	return false
}

// Convert int atom to float atom
@require_results
atom_to_float :: proc (atom: ^Atom, loc := #caller_location) -> ^Atom {
	if atom.kind == .Int {
		return atom_float(f64(atom.int), atom, loc)
	}
	return atom
}

@require_results
atom_copy :: proc (src: Atom, loc := #caller_location) -> (dst: Atom)
{
	dst = src
	#partial switch src.kind {
	case .Add, .Div, .Mul, .Pow:
		dst.lhs = atom_new(atom_copy(dst.lhs^, loc))
		dst.rhs = atom_new(atom_copy(dst.rhs^, loc))
	}
	return dst
}

@require_results
atom_add_if_possible :: proc (a, b: ^Atom) -> (sum: ^Atom, ok: bool)
{
	return visit_a_b(a, b)

	visit_a_b :: proc (a, b: ^Atom) -> (res: ^Atom, ok: bool)
	{
		// Check for distribution over or expressions first
		if result, distributed := distribute_over_or(.Add, a, b); distributed {
			return result, true
		}
		
		// 1 + 2  ->  3
		if a.kind == .Int && b.kind == .Int {
			return atom_num(a.int+b.int, a), true
		}
		else if a.kind == .Float && b.kind == .Float {
			return atom_num(a.float+b.float, a), true
		}
		else if a.kind == .Int && b.kind == .Float {
			return atom_num(f64(a.int)+b.float, a), true
		}
		else if a.kind == .Float && b.kind == .Int {
			return atom_num(a.float+f64(b.int), a), true
		}

		// "foo" + "bar"  ->  "foobar"
		if a.kind == .Str && b.kind == .Str {
			return atom_str(
				strings.concatenate({a.str, b.str}),
				a,
			), true
		}
 
		/*
			x * x    ->  2x
			2x * x   ->  3x
			x * 2x   ->  3x
			2x * 2x  ->  4x
		*/
		{
			atom_val_factor :: proc (atom: ^Atom) -> (val: ^Atom, f: ^Atom)
			{
				if atom.kind == .Mul {
					if atom.lhs.kind == .Int || atom.lhs.kind == .Float {
						return atom.rhs, atom.lhs
					} else if atom.rhs.kind == .Int || atom.rhs.kind == .Float {
						return atom.lhs, atom.rhs
					}
				}
				return atom, &atom_num_one
			}

			a_val, a_f := atom_val_factor(a)
			b_val, b_f := atom_val_factor(b)

			if atom_equals(a_val, b_val) {
				return atom_mul(
					a_val,
					atom_add(a_f, b_f),
				), true
			}
		}

		// a/b + c/d  ->  (ad + cb)/bd
		if a.kind == .Div && b.kind == .Div {
			return atom_div(
				atom_add(
					atom_mul(a.lhs, b.rhs),
					atom_mul(b.lhs, a.rhs),
				),
				atom_mul(a.rhs, b.rhs),
			), true
		}

		res, ok = visit_b(a, b)
		if ok do return
		res, ok = visit_b(b, a)
		return
	}
	
	visit_b :: proc (a, b: ^Atom) -> (res: ^Atom, ok: bool)
	{
		switch b.kind {
		case .Int:
			// x + 0  ->  x
			if b.int == 0 {
				return a, true
			}
		case .Float:
			// x + 0  ->  x
			if b.float == 0 {
				return a, true
			}
		case .Div:
			// a + (l/r)  ->  (a/1) + (l/r)  ->  (a*r + l)/r
			if ar, ok := atom_mul_if_possible(a, b.rhs); ok {
				return atom_div(atom_add(ar, b.lhs), b.rhs), true
			}
		case .Add:
			// (x + y) + x  ->  2x + y
			if new_lhs, ok := visit_a_b(b.lhs, a); ok {
				return atom_bin(.Add, new_lhs, b.rhs, b), true
			}
			// (y + x) + x  ->  y + 2x
			if new_rhs, ok := visit_a_b(b.rhs, a); ok {
				return atom_bin(.Add, b.lhs, new_rhs, b), true
			}
		case .Mul, .Pow, .Var, .Str, .Or, .Eq:
		}
		return
	}
}
@require_results
atom_add :: proc (a, b: ^Atom, loc := #caller_location) -> ^Atom {
	return atom_add_if_possible(a, b) or_else atom_bin(.Add, a, b, a, loc)
}
@require_results
atom_add_num :: proc (atom: ^Atom, f: f64, loc := #caller_location) -> ^Atom {
	return atom_add(atom, atom_float(f, atom, loc), loc)
}
@require_results
atom_sub :: proc (lhs, rhs: ^Atom, loc := #caller_location) -> ^Atom {
	return atom_add(lhs, atom_neg(rhs, loc), loc)
}

@require_results
atom_mul_if_possible :: proc (a, b: ^Atom) -> (product: ^Atom, ok: bool)
{
	return visit_a_b(a, b)

	visit_a_b :: proc (a, b: ^Atom) -> (res: ^Atom, ok: bool)
	{
		// Check for distribution over or expressions first
		if result, distributed := distribute_over_or(.Mul, a, b); distributed {
			return result, true
		}
		
		// 2 * 3  ->  6
		if a.kind == .Int && b.kind == .Int {
			return atom_num(a.int*b.int, a), true
		}
		else if a.kind == .Float && b.kind == .Float {
			return atom_num(a.float*b.float, a), true
		}
		else if a.kind == .Int && b.kind == .Float {
			return atom_num(f64(a.int)*b.float, a), true
		}
		else if a.kind == .Float && b.kind == .Int {
			return atom_num(a.float*f64(b.int), a), true
		}

		// x * x  ->  x^2
		if atom_equals(a^, b^) {
			return atom_pow(a, 2), true
		}

		// (a + b) * (c + d) -> a*c + a*d + b*c + b*d
		if a.kind == .Add && b.kind == .Add {
			return atom_add(
				atom_add(
					atom_mul(a.lhs, b.lhs),
					atom_mul(a.lhs, b.rhs),
				),
				atom_add(
					atom_mul(a.rhs, b.lhs),
					atom_mul(a.rhs, b.rhs),
				),
			), true
		}

		res, ok = visit_b(a, b)
		if ok do return
		res, ok = visit_b(b, a)
		return
	}
	

	visit_b :: proc (a, b: ^Atom) -> (res: ^Atom, ok: bool)
	{
		#partial switch b.kind {
		case .Int:
			switch b.int {
			case 0: return b, true // x * 0  ->  0
			case 1: return a, true // x * 1  ->  x
			}
		case .Float:
			switch b.float {
			case 0: return b, true // x * 0  ->  0
			case 1: return a, true // x * 1  ->  x
			}
		}

		// x^2 * x  ->  x^3
		// ? should other exponents beside num be allowed?
		if a.kind == .Pow && (a.rhs.kind == .Int || a.rhs.kind == .Float) && atom_equals(a.lhs, b) {
			return atom_bin(.Pow,
				a.lhs,
				atom_add(a.rhs, &atom_num_one),
				from=a,
			), true
		}

		if b.kind == .Int || b.kind == .Float {
			// ? 	All these beyond num are sus
			// ? probably need to use the atom_foo_if_possible procs here
			// ? and take it out from num rhs
			// ? 	Also usually you try to get the common factor out of an expression
			// ? not to it. eg `4a + 4b  ->  4(a + b)` (2 ops < 3 ops)
			#partial switch a.kind {
			// (a + b) * 4  ->  4a + 4b
			case .Add: return atom_add(
				atom_mul(a.lhs, b),
				atom_mul(a.rhs, b),
			), true
			// (a / b) * 4  ->  4a / b
			case .Div: return atom_bin(.Div,
				atom_mul(a.lhs, b),
				a.rhs,
				from=a,
			), true
			}
		}

		if a.kind == .Mul {
			// (x * y) * x  ->  x^2 * y
			if new_lhs, ok := visit_a_b(a.lhs, b); ok {
				return atom_bin(.Mul, new_lhs, a.rhs, from=a), true
			}
			// (y * x) * x  ->  y * x^2
			if new_rhs, ok := visit_a_b(a.rhs, b); ok {
				return atom_bin(.Mul, a.lhs, new_rhs, from=a), true
			}
		}

		return
	}
}
@require_results
atom_mul :: proc (a, b: ^Atom, loc := #caller_location) -> ^Atom {
	return atom_mul_if_possible(a, b) or_else atom_bin(.Mul, a, b, from=a, loc=loc)
}
@require_results
atom_mul_num :: proc (atom: ^Atom, f: f64, loc := #caller_location) -> ^Atom {
	return atom_mul(atom, atom_float(f, from=atom, loc=loc), loc)
}
@require_results
atom_neg :: proc (atom: ^Atom, loc := #caller_location) -> ^Atom {
	return atom_mul(atom, &atom_num_neg_one, loc)
}

@require_results
atom_div_if_possible :: proc (dividened, divisor: ^Atom) -> (quotient: ^Atom, ok: bool)
{
	// Check for distribution over or expressions first
	if result, distributed := distribute_over_or(.Div, dividened, divisor); distributed {
		return result, true
	}
	
	// 0/x  ->  0
	if atom_num_equals_zero(dividened^) {
		return dividened, true
	}

	// x/0  ->  Inf
	if atom_num_equals_zero(divisor^) {
		if dividened.kind == .Int {
			if dividened.int < 0 {
				return atom_num(math.inf_f64(-1), dividened), true
			} else {
				return atom_num(math.inf_f64(1), dividened), true
			}
		} else if dividened.kind == .Float {
			if dividened.float < 0 {
				return atom_num(math.inf_f64(-1), dividened), true
			} else {
				return atom_num(math.inf_f64(1), dividened), true
			}
		}
		return dividened, false
	}
	// x/1  ->  x
	if atom_num_equals_one(divisor^) {
		return dividened, true
	}

	// 6/3  ->  2
	if divisor.kind == .Int && dividened.kind == .Int {
		if dividened.int % divisor.int != 0 {
			return atom_num(f64(dividened.int)/f64(divisor.int), dividened), true
		}
		return atom_num(dividened.int/divisor.int, dividened), true
	}
	else if divisor.kind == .Float && dividened.kind == .Float {
		return atom_num(dividened.float/divisor.float, dividened), true
	}
	else if divisor.kind == .Int && dividened.kind == .Float {
		return atom_num(dividened.float/f64(divisor.int), dividened), true
	}
	else if divisor.kind == .Float && dividened.kind == .Int {
		return atom_num(f64(dividened.int)/divisor.float, dividened), true
	}

	// x/x  ->  1
	if atom_equals(dividened, divisor) {
		return &atom_num_one, true
	}

	#partial switch dividened.kind {
	// x*2 / x  ->  1*2  ->  2
	case .Mul:
		if lhs, ok := atom_div_if_possible(dividened.lhs, divisor); ok {
			return atom_mul(lhs, dividened.rhs), true
		}
		if rhs, ok := atom_div_if_possible(dividened.rhs, divisor); ok {
			return atom_mul(dividened.lhs, rhs), true
		}
	// (a+b)/c  ->  a/c + b/c
	case .Add:
		return atom_add(
			atom_div_if_possible(dividened.lhs, divisor) or_break,
			atom_div_if_possible(dividened.rhs, divisor) or_break,
		), true
	case .Div:
		// (x/y)/x  ->  1/y
		if lhs, ok := atom_div_if_possible(dividened.lhs, divisor); ok {
			return atom_div(lhs, dividened.rhs), true
		}
		// (a/b)/c  ->  a/(b*c)
		if rhs, ok := atom_mul_if_possible(dividened.rhs, divisor); ok {
			return atom_div(dividened.lhs, rhs), true
		}
	case .Pow:
		// x^3 / x  ->  x^2
		if atom_equals(dividened.lhs, divisor) {
			if dividened.rhs.kind == .Int {
				return atom_pow(dividened.lhs, dividened.rhs.int-1), true
			} else if dividened.rhs.kind == .Float {
				return atom_pow(dividened.lhs, dividened.rhs.float-1), true
			}
		}
	}
	
	return dividened, false
}
@require_results
atom_div :: proc (dividened, divisor: ^Atom, loc := #caller_location) -> ^Atom
{
	quotient, ok := atom_div_if_possible(dividened, divisor)
	if !ok {
		quotient = atom_bin(.Div, dividened, divisor, dividened, loc)
	}
	return quotient
}
@require_results
atom_div_num :: proc (dividened: ^Atom, f: f64, loc := #caller_location) -> ^Atom {
	switch f {
	case 0: return atom_num(math.inf_f64(-1), dividened)
	case 1: return atom_num(math.inf_f64(1), dividened)
	}
	return atom_div(dividened, atom_float(f, dividened, loc), loc)
}
@require_results
atom_flip :: proc (atom: ^Atom) -> ^Atom {
	#partial switch atom.kind {
	case .Div:   return atom_div(atom.rhs, atom.lhs)
	case .Int:   return atom_num(1/f64(atom.int), atom)
	case .Float: return atom_num(1/atom.float, atom)
	case:        return atom_div(&atom_num_one, atom)
	}
}

// Different from div_if_possible in that it has to completely remove the var from atom
@require_results
atom_div_extract_var_if_possible :: proc (atom: ^Atom, var: string) -> (res: ^Atom, ok: bool)
{
	res = atom // fallback

	switch atom.kind {
	case .Var:
		if atom.var == var {
			return &atom_num_one, true
		}
	case .Add:
		lhs := atom_div_extract_var_if_possible(atom.lhs, var) or_return
		rhs := atom_div_extract_var_if_possible(atom.rhs, var) or_return
		return atom_add(lhs, rhs), true
	case .Div:
		if !has_dependency(atom.rhs^, var) {
			lhs := atom_div_extract_var_if_possible(atom.lhs, var) or_return
			return atom_div(lhs, atom.rhs), true
		}
	case .Mul:
		if lhs, ok := atom_div_extract_var_if_possible(atom.lhs, var); ok {
			if !has_dependency(atom.rhs^, var) {
				return atom_mul(lhs, atom.rhs), true
			}
		} else if rhs, ok := atom_div_extract_var_if_possible(atom.rhs, var); ok {
			return atom_mul(atom.lhs, rhs), true
		}
	case .Pow, .Int, .Float, .Str, .Or, .Eq:
		// skip
	}

	return
}

@require_results
has_dependencies :: proc (atom: Atom) -> bool {
	if atom_is_binary(atom) {
		return has_dependencies(atom.lhs^) ||
		       has_dependencies(atom.rhs^)
	}
	return atom.kind == .Var
}
@require_results
has_dependency :: proc (atom: Atom, var: string) -> bool {
	if atom_is_binary(atom) {
		return has_dependency(atom.lhs^, var) ||
		       has_dependency(atom.rhs^, var)
	}
	return atom.kind == .Var && atom.var == var
}
@require_results
has_dependency_other_than_var :: proc (atom: Atom, var: string) -> bool {
	if atom_is_binary(atom) {
		return has_dependency_other_than_var(atom.lhs^, var) ||
		       has_dependency_other_than_var(atom.rhs^, var)
	}
	return atom.kind == .Var && atom.var != var
}

@require_results
atom_pow_if_possible :: proc (base, exponent: ^Atom) -> (pow: ^Atom, ok: bool) {
	// Check for distribution over or expressions first
	if result, distributed := distribute_over_or(.Pow, base, exponent); distributed {
		return result, true
	}

	// 2^0  ->  1
	if atom_num_equals_zero(exponent^) {
		return &atom_num_one, true
	}
	// 2^1  ->  2
	if atom_num_equals_one(exponent^) {
		return base, true
	}

	// 2^3  ->  8
	if exponent.kind == .Float && base.kind == .Float {
		return atom_num(math.pow(base.float, exponent.float), base), true
	}
	else if exponent.kind == .Int && base.kind == .Int {
		if res, ok := pow_int(base.int, exponent.int); ok {
			return atom_num(res, base), true
		}
		return atom_num(math.pow(f64(base.int), f64(exponent.int)), base), true
	}
	else if exponent.kind == .Int && base.kind == .Float {
		return atom_num(math.pow(base.float, f64(exponent.int)), base), true
	}
	else if exponent.kind == .Float && base.kind == .Int {
		return atom_num(math.pow(f64(base.int), exponent.float), base), true
	}

	return
}
@require_results
atom_pow_atom :: proc (base, exponent: ^Atom) -> ^Atom {
	return atom_pow_if_possible(base, exponent) or_else
	       atom_bin(.Pow, base, exponent, base)
}
@require_results
atom_pow_float :: proc (base: ^Atom, f: f64) -> ^Atom {
	return atom_pow_atom(base, atom_num(f))
}
@require_results
atom_pow_int :: proc (base: ^Atom, f: int) -> ^Atom {
	return atom_pow_atom(base, atom_num(f))
}
atom_pow :: proc {atom_pow_atom, atom_pow_float, atom_pow_int}

@require_results
atom_or_if_possible :: proc (lhs, rhs: ^Atom) -> (or_expr: ^Atom, ok: bool) {
	// No simplification for basic or expressions - just return as is
	// The real work happens when operations are applied to or expressions
	return
}

@require_results
atom_or :: proc (a, b: ^Atom, loc := #caller_location) -> ^Atom {
	return atom_or_if_possible(a, b) or_else atom_bin(.Or, a, b, a, loc)
}

// Helper function to distribute operations over or expressions
@require_results
distribute_over_or :: proc (op_kind: Atom_Kind, a, b: ^Atom) -> (result: ^Atom, ok: bool)
{
	// If 'a' is an or expression: (x | y) op b -> (x op b) | (y op b)
	if a.kind == .Or {
		return atom_or(
			atom_bin(op_kind, a.lhs, b, a),
			atom_bin(op_kind, a.rhs, b, a),
		), true
	}
	// If 'b' is an or expression: a op (x | y) -> (a op x) | (a op y)
	if b.kind == .Or {
		return atom_or(
			atom_bin(op_kind, a, b.lhs, b),
			atom_bin(op_kind, a, b.rhs, b),
		), true
	}
	
	return
}

atom_eq_if_possible :: proc (lhs, rhs: ^Atom, var: string) -> (eq: ^Atom, updated: bool)
{
	lhs, rhs := lhs, rhs

	/*
	move addends if they do(n't) depend on var
	*/
	// 1+2+x = y  ->  x = y-1-2
	if res, ok := move_addends(lhs, &rhs, var, false); ok {
		lhs = res
		updated = true
	}
	// x = 1+2-x  ->  x+x = 1+2
	if res, ok := move_addends(rhs, &lhs, var, true); ok {
		rhs = res
		updated = true
	}

	move_addends :: proc (atom: ^Atom, dst: ^^Atom, var: string, cond: bool) -> (res: ^Atom, ok: bool)
	{
		if atom.kind == .Add {
			lhs, lhs_ok := move_addends(atom.lhs, dst, var, cond)
			rhs, rhs_ok := move_addends(atom.rhs, dst, var, cond)
			if lhs_ok || rhs_ok {
				return atom_add(lhs, rhs), true
			}
		} else if has_dependency(atom^, var) == cond {
			dst^ = atom_sub(dst^, atom)
			return &atom_num_zero, true
		}
		return atom, false
	}

	/*
		^	^	^	^	^	^	^	^	^	^
	TODO these could be merged together probably
		V	V	V	V	V	V	V	V	V	V
	*/

	#partial switch lhs.kind {
	case .Div:
		// x/2 = y  ->  x = y*2
		lhs, rhs = lhs.lhs, atom_mul(rhs, lhs.rhs)
		updated = true

	case .Pow:
		/*
		move exponent to the right
		x^2 = y  ->  x = y^(1/2)
		*/
		if !has_dependency(lhs.rhs^, var) {
			lhs, rhs = lhs.lhs, atom_pow_atom(rhs, atom_flip(lhs.rhs))
			updated = true
		}

	case .Mul:
		/*
		move factors to rhs
		2 * x = 1  ->  x = 1/2
		*/
		if res, ok := move_factors(lhs, &rhs, var); ok {
			lhs = res
			updated = true
		}

		move_factors :: proc (atom: ^Atom, dst: ^^Atom, var: string) -> (res: ^Atom, updated: bool)
		{
			if atom.kind == .Mul {
				lhs, lhs_ok := move_factors(atom.lhs, dst, var)
				rhs, rhs_ok := move_factors(atom.rhs, dst, var)
				if lhs_ok || rhs_ok {
					return atom_mul(lhs, rhs), true
				}
			} else if !has_dependency(atom^, var) {
				dst^ = atom_div(dst^, atom)
				return &atom_num_one, true
			}
			return atom, false
		}

	case .Add:
		/*
		extract var and divide rhs
		2a + 3ab = y  ->  a(2 + 3b) = y  ->  a = y / (2 + 3b)
		*/
		if div, ok := atom_div_extract_var_if_possible(lhs, var); ok {
			lhs, rhs = atom_var(var), atom_div(rhs, div)
			updated = true
		}
	}

	if updated {
		eq = atom_bin(.Eq, lhs, rhs, lhs)
	}

	return
}

// Compares structurally
@require_results
atom_equals_val :: proc (a, b: Atom) -> bool
{
	if a.kind == b.kind {
		switch a.kind {
		case .Int:   return a.int == b.int
		case .Float: return a.float == b.float
		case .Str:   return a.str == b.str
		case .Var:   return a.var == b.var
		case .Add, .Div, .Mul, .Pow, .Or, .Eq:
			return atom_equals(a.lhs, b.lhs) &&
				   atom_equals(a.rhs, b.rhs)
		}
	}

	return false
}
@require_results
atom_equals_ptr :: proc (a, b: ^Atom) -> bool {
	return a == b || atom_equals(a^, b^)
}
@require_results
atom_equals :: proc{atom_equals_val, atom_equals_ptr}

// Compares structurally
@require_results
constraint_equals :: proc (a, b: Constraint) -> bool {
	return a.var == b.var && atom_equals(a.atom, b.atom)
}

_unused_updated: bool

fold_atom :: proc (atom: ^^Atom, var: string) -> (updated: bool)
{
	for atom_is_binary(atom^^) {

		lhs, rhs := atom^.lhs, atom^.rhs
		lhs_updated := fold_atom(&lhs, var)
		rhs_updated := fold_atom(&rhs, var)

		res: ^Atom; bin_updated: bool
		switch atom^.kind {
		case .Add: res, bin_updated = atom_add_if_possible(lhs, rhs)
		case .Mul: res, bin_updated = atom_mul_if_possible(lhs, rhs)
		case .Div: res, bin_updated = atom_div_if_possible(lhs, rhs)
		case .Pow: res, bin_updated = atom_pow_if_possible(lhs, rhs)
		case .Eq:  res, bin_updated = atom_eq_if_possible(lhs, rhs, var)
		case .Or:  res, bin_updated = atom_or_if_possible(lhs, rhs)
		case .Int, .Float, .Str, .Var: unreachable()
		}

		if bin_updated {
			// If the bin was updated, then we can use it
			atom^ = res
			updated = true
		} else if lhs_updated || rhs_updated {
			// If lhs or rhs were updated, but not the bin itself
			// then we still need to update the atom
			atom^ = atom_bin(atom^.kind, lhs, rhs, atom^)
			updated = true
			break
		} else {
			// If nothing was updated, then we can break the loop
			break
		}
	}

	return
}

try_substituting_var :: proc (atom: ^Atom, var: string, value: ^Atom) -> (res: ^Atom, ok: bool)
{
	if atom_val_equals(atom^, var) {
		return value, true
	}

	if atom_is_binary(atom^) {
		lhs, lhs_ok := try_substituting_var(atom.lhs, var, value)
		rhs, rhs_ok := try_substituting_var(atom.rhs, var, value)
		if lhs_ok || rhs_ok {
			return atom_bin(atom.kind, lhs, rhs, atom), true
		}
	}

	return atom, false
}

@require_results
is_constraint_solved :: proc (constr: Constraint) -> bool {
	if constr.atom.kind == .Eq {
		return atom_val_equals(constr.atom.lhs^, constr.var) &&
		       !has_dependencies(constr.atom.rhs^)
	}
	return false
}

// Compares vars by value if they do not contradict
// constraint_contradicts :: proc (a, b: Constraint) -> bool
// {
// 	a_var, is_a_lhs_var := a.lhs.(Atom_Var)
// 	b_var, is_b_lhs_var := b.lhs.(Atom_Var)
// 	a_val, is_a_lhs_num := a.lhs.(Atom_Num)
// 	b_val, is_b_lhs_num := b.lhs.(Atom_Num)

// 	return !(
// 		a.var == b.var &&
// 		is_a_lhs_var &&
// 		is_b_lhs_var &&
// 		a_var.name == b_var.name &&
// 		is_a_lhs_num &&
// 		is_b_lhs_num &&
// 		a_val.f == b_val.f \
// 	)
// }

atom_from_expr :: proc (expr: Expr) -> (a: ^Atom)
{
	switch v in expr {
	case ^Expr_Single:

		#partial switch v.token.kind {
		case .Float:
			value, _ := expr_single_float_value(v^)
			// TODO: handle errors
			return atom_float(value)
		case .Int:
			value, _ := expr_single_int_value(v^)
			// TODO: handle errors
			return atom_int(value)
		case .Str:
			value, _ := expr_single_string_value(v^)
			// TODO: handle errors
			return atom_str(value)
		case .Ident:
			name := expr_single_ident_value(v^)
			a = atom_var(name)
		case:
			unreachable()
		}
		
	case ^Expr_Unary:
		a = atom_from_expr(v.rhs)
		if v.op_token.kind == .Sub {
			a = atom_neg(a)
		}

	case ^Expr_Binary:
		a = atom_new({
			lhs = atom_from_expr(v.lhs),
			rhs = atom_from_expr(v.rhs),
		})
		
		#partial switch v.op_token.kind {
		case .Add: a.kind = .Add
		case .Sub: a.kind = .Add
		           a.rhs  = atom_neg(a.rhs)
		case .Mul: a.kind = .Mul
		case .Div: a.kind = .Div
		case .Pow: a.kind = .Pow
		case .Eq:  a.kind = .Eq
		case .Or:  a.kind = .Or
		case:
			unreachable()
		}
	}

	return
}

@require_results
constraints_from_exprs :: proc (exprs: []Expr, allocator := context.allocator) -> []Constraint
{
	context.allocator = allocator

	atoms := slice.mapper(exprs, atom_from_expr, context.temp_allocator)
	defer delete(atoms, context.temp_allocator)

	constrs := make([dynamic]Constraint, 0, 16, allocator)
	defer shrink(&constrs)

	for atom in atoms {
		// Add constr for each var in atom,
		// clone for subsequent vars
		// and exclude vars that are already in constrs

		_visit(atom, atom^, &constrs, len(constrs))
		_visit :: proc (atom: ^Atom, root_atom: Atom, constrs: ^[dynamic]Constraint, start_idx: int)
		{
			if atom.kind == .Var {
				// Check if var is already in constrs
				for c in constrs[start_idx:] {
					if c.var == atom.var do return
				}

				// Add new constraint for this var
				append(constrs, Constraint{
					var  = atom.var,
					atom = atom_new(root_atom if len(constrs) == start_idx else atom_copy(root_atom)),
				})
			} else if atom_is_binary(atom^) {
				_visit(atom.lhs, root_atom, constrs, start_idx)
				_visit(atom.rhs, root_atom, constrs, start_idx)
			}
		}
	}

	return constrs[:]
}

solve :: proc (constrs: []Constraint, allocator := context.allocator)
{
	context.allocator = allocator

	// scratch_allocator := mem.Scratch_Allocator{}
	// mem.scratch_allocator_init(&scratch_allocator, mem.Megabyte, context.temp_allocator)
	// context.temp_allocator = mem.scratch_allocator(&scratch_allocator)
	// defer mem.scratch_allocator_destroy(&scratch_allocator)

	solve_loop: for {
		// free_all(context.temp_allocator)

		updated: bool

		for &constr in constrs {
			fold_atom(&constr.atom, constr.var) or_continue
			updated = true
		}

		if updated do continue

		for &constr, constr_i in constrs {
			
			if is_constraint_solved(constr) {

				assert(constr.atom.kind == .Eq, "constraint should be solved with equality")
				assert(constr.atom.lhs.kind == .Var, "lhs of solved constraint should be a var")

				// try substituting solved vars
				for &constr2, constr2_i in constrs {
					if constr_i != constr2_i && constr.var != constr2.var {
						atom, ok := try_substituting_var(constr2.atom, constr.var, constr.atom.rhs)
						constr2.atom = atom
						updated ||= ok
					}
				}
			} else {
				// try approximation
				try_finding_polynomial_solution(&constr, &updated)
			}

			if updated {
				continue solve_loop
			}
		}
		
		break
	}
}

try_finding_polynomial_solution :: proc (constr: ^Constraint, updated: ^bool)
{
	context.allocator = context.temp_allocator

	// Only support equality constraints for now
	eq := constr.atom
	if eq.kind != .Eq do return

	if has_dependency_other_than_var(eq^, constr.var) do return

	/*
	move right to the left to have a single atom equals 0
	x^2 + x = 12  ->  x^2 + x + -12 = 0
	*/
	atom := atom_sub(eq.lhs, eq.rhs)
	fold_atom(&atom, constr.var)

	poly, ok := polynomial_from_atom(atom^, constr.var)
	if !ok do return

	root, found := find_polynomial_root(poly)
	if found {
		eq.lhs = atom_var(constr.var, eq.lhs)
		eq.rhs = atom_num(root, eq.rhs)
		updated^   = true
	}
}

// 1. solve
// 2. remove duplicates
// 3. return false if contradicting
resolve :: proc (constrs_in: []Constraint, allocator := context.allocator) ->
	(constrs_out: []Constraint, solved: bool, ok: bool)
{
	context.allocator = allocator

	solve(constrs_in, allocator)

	constrs_dyn := make([dynamic]Constraint, 0, len(constrs_in))
	defer shrink(&constrs_dyn)

	solved = true
	ok = true

	outer: for constr in constrs_in {

		// remove duplicates
		for &c in constrs_dyn {
			if constraint_equals(constr, c) {
				continue outer
			}
		}
		
		if is_constraint_solved(constr) {

			assert(constr.atom.kind == .Eq, "constraint should be solved with equality")
			assert(constr.atom.lhs.kind == .Var, "lhs of solved constraint should be a var")

			// check for contradictions
			for &c in constrs_dyn {
				if constr.var == c.var &&
				   is_constraint_solved(c) &&
				   !atom_equals(constr.atom, c.atom) {
					ok = false
				}
			}
		} else {
			solved = false
		}

		append(&constrs_dyn, constr)
	}

	return constrs_dyn[:], solved, ok
}
