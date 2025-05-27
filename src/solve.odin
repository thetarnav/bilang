package bilang

import "base:runtime"
import "base:intrinsics"

import "core:math"
import "core:strings"

import "../utils"

/*
atom pointers can be repeated, (same pointers in multiple places)
so the value of atom behind a pointer must stay the same,
when changing the value a new atom must be allocated and pointer changed
*/
Atom :: struct {
	kind: Atom_Kind,
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
}

ATOM_NUM_KINDS    :: bit_set[Atom_Kind]{.Int, .Float}
ATOM_BINARY_KINDS :: bit_set[Atom_Kind]{.Add, .Div, .Mul, .Pow}

Constraint :: struct {
	var:      ^Atom,
	lhs, rhs: ^Atom,
}

@require_results
atom_new :: proc (atom: Atom, loc := #caller_location) -> ^Atom {
	a, err := new(Atom, loc=loc)
	// TODO: setup a separate allocator for errors
	utils.alloc_error_assert("atom_new error: ", err, loc)
	a^ = atom
	return a
}

atom_num_zero    := Atom{kind=.Int, int= 0}
atom_num_one     := Atom{kind=.Int, int= 1}
atom_num_neg_one := Atom{kind=.Int, int=-1}

@require_results
atom_int :: proc (val: int, loc := #caller_location) -> ^Atom {
	switch val {
	case  0: return &atom_num_zero
	case  1: return &atom_num_one
	case -1: return &atom_num_neg_one
	case:    return atom_new({kind=.Int, int=val}, loc)
	}
}
@require_results
atom_float :: proc (val: f64, loc := #caller_location) -> ^Atom {
	return atom_new({kind=.Float, float=val}, loc)
}
@require_results
atom_str :: proc (val: string, loc := #caller_location) -> ^Atom {
	return atom_new({kind=.Str, var=val}, loc)
}
@require_results
atom_var :: proc (var: string, loc := #caller_location) -> ^Atom {
	return atom_new({kind=.Var, var=var}, loc)
}
@require_results
atom_binary :: proc (kind: Atom_Kind, lhs, rhs: ^Atom, loc := #caller_location) -> ^Atom {
	return atom_new({kind=kind, lhs=lhs, rhs=rhs}, loc)
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
		return atom_float(f64(atom.int), loc)
	}
	return atom
}

@require_results
atom_copy :: proc (src: Atom, loc := #caller_location) -> Atom
{
	#partial switch src.kind {
	case .Add, .Div, .Mul, .Pow:
		dst := src
		dst.lhs = atom_new(atom_copy(dst.lhs^, loc), loc)
		dst.rhs = atom_new(atom_copy(dst.rhs^, loc), loc)
	}
	return src
}

@require_results
atom_add_if_possible :: proc (a, b: ^Atom) -> (sum: ^Atom, ok: bool)
{
	return visit_a_b(a, b)

	visit_a_b :: proc (a, b: ^Atom) -> (res: ^Atom, ok: bool)
	{
		// 1 + 2  ->  3
		if a.kind == .Int && b.kind == .Int {
			return atom_num(a.int+b.int), true
		}
		else if a.kind == .Float && b.kind == .Float {
			return atom_num(a.float+b.float), true
		}
		else if a.kind == .Int && b.kind == .Float {
			return atom_num(f64(a.int)+b.float), true
		}
		else if a.kind == .Float && b.kind == .Int {
			return atom_num(a.float+f64(b.int)), true
		}

		// "foo" + "bar"  ->  "foobar"
		if a.kind == .Str && b.kind == .Str {
			return atom_str(
				strings.concatenate({a.str, b.str}),
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
				return atom_binary(.Add, new_lhs, b.rhs), true
			}
			// (y + x) + x  ->  y + 2x
			if new_rhs, ok := visit_a_b(b.rhs, a); ok {
				return atom_binary(.Add, b.lhs, new_rhs), true
			}
		case .Mul, .Pow, .Var, .Str:
		}
		return
	}
}
@require_results
atom_add :: proc (a, b: ^Atom, loc := #caller_location) -> ^Atom {
	return atom_add_if_possible(a, b) or_else atom_binary(.Add, a, b, loc)
}
@require_results
atom_add_num :: proc (atom: ^Atom, f: f64, loc := #caller_location) -> ^Atom {
	return atom_add(atom, atom_float(f, loc), loc)
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
		// 2 * 3  ->  6
		if a.kind == .Int && b.kind == .Int {
			return atom_num(a.int*b.int), true
		}
		else if a.kind == .Float && b.kind == .Float {
			return atom_num(a.float*b.float), true
		}
		else if a.kind == .Int && b.kind == .Float {
			return atom_num(f64(a.int)*b.float), true
		}
		else if a.kind == .Float && b.kind == .Int {
			return atom_num(a.float*f64(b.int)), true
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
			return atom_binary(.Pow,
				a.lhs,
				atom_add(a.rhs, &atom_num_one),
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
			case .Div: return atom_binary(.Div,
				atom_mul(a.lhs, b),
				a.rhs,
			), true
			}
		}

		if a.kind == .Mul {
			// (x * y) * x  ->  x^2 * y
			if new_lhs, ok := visit_a_b(a.lhs, b); ok {
				return atom_binary(.Mul, new_lhs, a.rhs), true
			}
			// (y * x) * x  ->  y * x^2
			if new_rhs, ok := visit_a_b(a.rhs, b); ok {
				return atom_binary(.Mul, a.lhs, new_rhs), true
			}
		}

		return
	}
}
@require_results
atom_mul :: proc (a, b: ^Atom, loc := #caller_location) -> ^Atom {
	return atom_mul_if_possible(a, b) or_else atom_binary(.Mul, a, b, loc)
}
@require_results
atom_mul_num :: proc (atom: ^Atom, f: f64, loc := #caller_location) -> ^Atom {
	return atom_mul(atom, atom_float(f, loc), loc)
}
@require_results
atom_neg :: proc (atom: ^Atom, loc := #caller_location) -> ^Atom {
	return atom_mul(atom, &atom_num_neg_one, loc)
}

@require_results
atom_div_if_possible :: proc (dividened, divisor: ^Atom) -> (quotient: ^Atom, ok: bool)
{
	// 0/x  ->  0
	if atom_num_equals_zero(dividened^) {
		return dividened, true
	}

	// x/0  ->  X_X
	if atom_num_equals_zero(divisor^) {
		return dividened, false
	}
	// x/1  ->  x
	if atom_num_equals_one(divisor^) {
		return dividened, true
	}

	// 6/3  ->  2
	if divisor.kind == .Int && dividened.kind == .Int {
		if dividened.int % divisor.int != 0 {
			return atom_num(f64(dividened.int)/f64(divisor.int)), true
		}
		return atom_num(dividened.int/divisor.int), true
	}
	else if divisor.kind == .Float && dividened.kind == .Float {
		return atom_num(dividened.float/divisor.float), true
	}
	else if divisor.kind == .Int && dividened.kind == .Float {
		return atom_num(dividened.float/f64(divisor.int)), true
	}
	else if divisor.kind == .Float && dividened.kind == .Int {
		return atom_num(f64(dividened.int)/divisor.float), true
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
		assert(!atom_num_equals_zero(divisor^), "division by zero", loc)
		quotient = atom_binary(.Div, dividened, divisor, loc)
	}
	return quotient
}
@require_results
atom_div_num :: proc (dividened: ^Atom, f: f64, loc := #caller_location) -> ^Atom {
	return atom_div(dividened, atom_num(f, loc), loc)
}
@require_results
atom_flip :: proc (atom: ^Atom) -> ^Atom {
	#partial switch atom.kind {
	case .Div:   return atom_div(atom.rhs, atom.lhs)
	case .Int:   return atom_num(1/f64(atom.int))
	case .Float: return atom_num(1/atom.float)
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
	case .Pow, .Int, .Float, .Str:
		// skip
	}

	return
}

@require_results
has_dependencies :: proc (atom: Atom) -> bool {
	switch atom.kind {
	case .Int, .Float, .Str:
		return false
	case .Var:
		return true
	case .Add, .Div, .Mul, .Pow:
		return has_dependencies(atom.lhs^) ||
		       has_dependencies(atom.rhs^)
	}
	return false
}
@require_results
has_dependency :: proc (atom: Atom, var: string) -> bool {
	switch atom.kind {
	case .Int, .Float, .Str:
		return false
	case .Var:
		return atom.var == var
	case .Add, .Div, .Mul, .Pow:
		return has_dependency(atom.lhs^, var) ||
		       has_dependency(atom.rhs^, var)
	}
	return false
}
@require_results
has_dependency_other_than_var :: proc (atom: Atom, var: string) -> bool {
	switch atom.kind {
	case .Int, .Float, .Str:
	    return false
	case .Var:
		return atom.var != var
	case .Add, .Div, .Mul, .Pow:
		return has_dependency_other_than_var(atom.lhs^, var) ||
		       has_dependency_other_than_var(atom.rhs^, var)
	}
	return false
}

@require_results
atom_pow_if_possible :: proc (base, exponent: ^Atom) -> (pow: ^Atom, ok: bool) {

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
		return atom_num(math.pow(base.float, exponent.float)), true
	}
	else if exponent.kind == .Int && base.kind == .Int {
		if res, ok := pow_int(base.int, exponent.int); ok {
			return atom_num(res), true
		}
		return atom_num(math.pow(f64(base.int), f64(exponent.int))), true
	}
	else if exponent.kind == .Int && base.kind == .Float {
		return atom_num(math.pow(base.float, f64(exponent.int))), true
	}
	else if exponent.kind == .Float && base.kind == .Int {
		return atom_num(math.pow(f64(base.int), exponent.float)), true
	}

	return
}
@require_results
atom_pow_atom :: proc (base, exponent: ^Atom) -> ^Atom {
	return atom_pow_if_possible(base, exponent) or_else
	       atom_binary(.Pow, base, exponent)
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
		case .Add, .Div, .Mul, .Pow:
			return atom_equals_ptr(a.lhs, b.lhs) &&
				   atom_equals_ptr(a.rhs, b.rhs)
		}
	}

	return false
}
@require_results
atom_equals_ptr :: proc (a, b: ^Atom) -> bool {
	return a == b || atom_equals_val(a^, b^)
}
@require_results
atom_equals :: proc{atom_equals_val, atom_equals_ptr}

// Compares structurally
@require_results
contraint_equals :: proc (a, b: Constraint) -> bool {
	return a.var == b.var && atom_equals(a.lhs, b.lhs) && atom_equals(a.rhs, b.rhs)
}

_dummy_updated: bool

fold_atom :: proc (atom: ^^Atom, updated: ^bool)
{
	for res in fold_atom_once(atom^) {
		atom^    = res
		updated^ = true
		log_debug("folding atom")
	}

	fold_atom_once :: proc (atom: ^Atom) -> (res: ^Atom, updated: bool)
	{
		res = atom

		if !atom_is_binary(atom^) {
			return
		}

		fold_atom(&atom.lhs, &updated)
		fold_atom(&atom.rhs, &updated)
		
		#partial switch atom.kind {
		case .Add: res = atom_add_if_possible(atom.lhs, atom.rhs) or_return
		case .Mul: res = atom_mul_if_possible(atom.lhs, atom.rhs) or_return
		case .Div: res = atom_div_if_possible(atom.lhs, atom.rhs) or_return
		case .Pow: res = atom_pow_if_possible(atom.lhs, atom.rhs) or_return
		}

		return res, true
	}
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
			return atom_binary(atom.kind, lhs, rhs), true
		}
	}

	return atom, false
}

@require_results
is_constraint_solved :: proc (constr: Constraint) -> bool {
	return atom_val_equals(constr.lhs^, constr.var.var) && !has_dependencies(constr.rhs^)
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

@require_results
constraints_from_decls :: proc (decls: []Decl, allocator := context.allocator) -> []Constraint
{
	context.allocator = allocator

	constrs := make([dynamic]Constraint, 0, 16)
	defer shrink(&constrs)

	for decl in decls {
		decl_start := len(constrs)

		lhs := atom_from_expr(decl.lhs, &constrs, decl_start)
		rhs := atom_from_expr(decl.rhs, &constrs, decl_start)
		
		for &constr in constrs[decl_start:] {
			constr.lhs, constr.rhs = lhs, rhs
		}
	}

	return constrs[:]

	atom_from_expr :: proc (
		expr:            Expr,
		all_constraints: ^[dynamic]Constraint,
		decl_start:      int,                  // index into all_constraints
	) -> (a: ^Atom)
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
				
				for c in all_constraints[decl_start:] {
					if c.var.var == name do return a
				}
				// var not in decl constraints
				append(all_constraints, Constraint{var = a})
			case:
				unreachable()
			}
			
		case ^Expr_Unary:
			a = atom_from_expr(v.rhs, all_constraints, decl_start)
			if v.op_token.kind == .Sub {
				a = atom_neg(a)
			}

		case ^Expr_Binary:
			a = atom_new({
				lhs = atom_from_expr(v.lhs, all_constraints, decl_start),
				rhs = atom_from_expr(v.rhs, all_constraints, decl_start),
			})
			
			#partial switch v.op_token.kind {
			case .Add: a.kind = .Add
			case .Sub: a.kind = .Add
			           a.rhs  = atom_neg(a.rhs)
			case .Mul: a.kind = .Mul
			case .Div: a.kind = .Div
			case .Pow: a.kind = .Pow
			}
		}

		return
	}
}

fold_constraint :: proc (constr_i: int, constrs: []Constraint, updated: ^bool)
{
	#no_bounds_check constr := &constrs[constr_i]
	
	fold_atom(&constr.lhs, updated)
	fold_atom(&constr.rhs, updated)

	/*
	move addends if they do(n't) depend on var
	*/
	// 1+2+x = y  ->  x = y-1-2
	if res, ok := move_addends(constr.lhs, &constr.rhs, constr.var.var, false); ok {
		constr.lhs = res
		updated^   = true
		log_debug("move addends to rhs")
	}
	// x = 1+2-x  ->  x+x = 1+2
	if res, ok := move_addends(constr.rhs, &constr.lhs, constr.var.var, true); ok {
		constr.rhs = res
		updated^   = true
		log_debug("move addends to lhs")
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

	#partial switch constr.lhs.kind {
	case .Div:
		// x/2 = y  ->  x = y*2
		constr.lhs, constr.rhs = constr.lhs.lhs, atom_mul(constr.rhs, constr.lhs.rhs)
		log_debug("moved lhs div to rhs")
		updated^ = true

	case .Pow:
		/*
		move exponent to the right
		x^2 = y  ->  x = y^(1/2)
		*/
		if !has_dependency(constr.lhs.rhs^, constr.var.var) {
			constr.lhs, constr.rhs = constr.lhs.lhs, atom_pow_atom(constr.rhs, atom_flip(constr.lhs.rhs))

			log_debug("moved exponent to rhs")
			updated^ = true
		}

	case .Mul:
		/*
		move factors to rhs
		2 * x = 1  ->  x = 1/2
		*/
		if res, ok := move_factors(constr.lhs, &constr.rhs, constr.var.var); ok {
			constr.lhs = res
			updated^   = true
			log_debug("move factors to rhs")
		}

		move_factors :: proc (atom: ^Atom, dst: ^^Atom, var: string) -> (res: ^Atom, ok: bool)
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
		if div, ok := atom_div_extract_var_if_possible(constr.lhs, constr.var.var); ok {
			constr.lhs, constr.rhs = constr.var, atom_div(constr.rhs, div)

			log_debug("extracting var")
			updated^ = true
		}
	}
}

solve :: proc (constrs: []Constraint, allocator := context.allocator)
{
	context.allocator = allocator

	/*
	Display additional information about current state of constraints when debugging
	*/
	when ODIN_DEBUG {
		Logger_Data :: struct {
			logger_proc: runtime.Logger_Proc,
			logger_data: rawptr,
			constrs:     []Constraint,
		}
		logger_data := Logger_Data{
			logger_proc = context.logger.procedure,
			logger_data = context.logger.data,
			constrs     = constrs,
		}
		context.logger.data = &logger_data
		context.logger.procedure = proc (
			data_raw: rawptr,
			level:    runtime.Logger_Level,
			text:     string,
			options:  runtime.Logger_Options,
			location: runtime.Source_Code_Location,
		) {
			data   := (^Logger_Data)(data_raw)
			output := constraints_to_string(data.constrs[:], {highlight=true}, context.temp_allocator)
			text   := strings.concatenate({text, "\n", output}, context.temp_allocator)
			data.logger_proc(data.logger_data, level, text, options, location)
		}
	}

	// scratch_allocator := mem.Scratch_Allocator{}
	// mem.scratch_allocator_init(&scratch_allocator, mem.Megabyte, context.temp_allocator)
	// context.temp_allocator = mem.scratch_allocator(&scratch_allocator)
	// defer mem.scratch_allocator_destroy(&scratch_allocator)

	solve_loop: for {
		// free_all(context.temp_allocator)

		updated: bool

		for _, constr_i in constrs {
			fold_constraint(constr_i, constrs[:], &updated)
		}

		if updated do continue

		for &constr, constr_i in constrs {
			
			if is_constraint_solved(constr) {
				// try substituting solved vars
				var := constr.lhs.var

				for &constr2, constr2_i in constrs {
					if constr_i == constr2_i || constr.var.var == constr2.var.var {
						continue
					}

					lhs, lhs_ok := try_substituting_var(constr2.lhs, var, constr.rhs)
					rhs, rhs_ok := try_substituting_var(constr2.rhs, var, constr.rhs)
					constr2.lhs, constr2.rhs = lhs, rhs
					updated ||= lhs_ok || rhs_ok
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

	if has_dependency_other_than_var(constr.lhs^, constr.var.var) ||
	   has_dependency_other_than_var(constr.rhs^, constr.var.var) {
		return
	}

	/*
	move right to the left to have a single atom equals 0
	x^2 + x = 12  ->  x^2 + x + -12 = 0
	*/
	atom := atom_sub(constr.lhs, constr.rhs)
	fold_atom(&atom, &_dummy_updated)

	poly, ok := polynomial_from_atom(atom^, constr.var.var)
	if !ok do return

	root, found := find_polynomial_root(poly)
	if found {
		constr.lhs = atom_var(constr.var.var)
		constr.rhs = atom_num(root)
		updated^   = true
		log_debug("found polynomial solution")
	}
}
