package bilang

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
	from: ^Atom, // previous atom from which this one was created
	using _: struct #raw_union {
		using bin: struct {lhs, rhs: ^Atom},
		get: struct {
			atom: ^Atom,
			name: string,
		},
		float: f64,
		int:   int,
		str:   string,
		var:   string,
	},
}

Atom_Kind :: enum u8 {
	None,
	Int,
	Float,
	Str,
	Var,
	Add,
	Mul,
	Div,
	Pow,
	Or,
	And,
	Eq,
	Get,
}

ATOM_NUM_KINDS    :: bit_set[Atom_Kind]{.Int, .Float}
ATOM_BINARY_KINDS :: bit_set[Atom_Kind]{.Add, .Div, .Mul, .Pow, .Or, .And, .Eq}

Constraints :: struct {
	vars:  map[string]^Atom,
	order: [dynamic]string,
}

atom_kind_to_token_kind :: proc (kind: Atom_Kind) -> Token_Kind {
	switch kind {
	case .Int:   return .Int
	case .Float: return .Float
	case .Str:   return .Str
	case .Var:   return .Ident
	case .Add:   return .Add
	case .Mul:   return .Mul
	case .Div:   return .Div
	case .Pow:   return .Pow
	case .Or:    return .Or
	case .And:   return .And
	case .Eq:    return .Eq
	case .Get, .None: fallthrough
	case:        return .Invalid
	}
}

@require_results
atom_new :: proc (atom: Atom, from: ^Atom = nil, loc := #caller_location) -> ^Atom {
	atom := atom
	atom.from = from
	a, err := new_val(atom, loc=loc)
	// TODO: setup a separate allocator for errors
	utils.alloc_error_assert("atom_new error: ", err, loc)
	return a
}

atom_none        := Atom{kind=.None}
atom_int_zero    := Atom{kind=.Int, int= 0}
atom_int_one     := Atom{kind=.Int, int= 1}
atom_int_neg_one := Atom{kind=.Int, int=-1}

@require_results
atom_new_none :: proc (from: ^Atom = nil, loc := #caller_location) -> ^Atom {
	return atom_new(atom_none, from=from, loc=loc) if from != nil else &atom_none
}
@require_results
atom_new_int :: proc (val: int, from: ^Atom = nil, loc := #caller_location) -> ^Atom {
	if from == nil do switch val {
	case  0: return &atom_int_zero
	case  1: return &atom_int_one
	case -1: return &atom_int_neg_one
	}
	return atom_new({kind=.Int, int=val}, from=from, loc=loc)
}
@require_results atom_new_float :: proc (val: f64,                         from: ^Atom = nil, loc := #caller_location) -> ^Atom {return atom_new({kind=.Float, float=val},                from=from, loc=loc)}
@require_results atom_new_str   :: proc (val: string,                      from: ^Atom = nil, loc := #caller_location) -> ^Atom {return atom_new({kind=.Str, var=val},                    from=from, loc=loc)}
@require_results atom_new_var   :: proc (var: string,                      from: ^Atom = nil, loc := #caller_location) -> ^Atom {return atom_new({kind=.Var, var=var},                    from=from, loc=loc)}
@require_results atom_new_bin   :: proc (kind: Atom_Kind, lhs, rhs: ^Atom, from: ^Atom = nil, loc := #caller_location) -> ^Atom {return atom_new({kind=kind, lhs=lhs, rhs=rhs},           from=from, loc=loc)}
@require_results atom_new_add   :: proc (lhs, rhs: ^Atom,                  from: ^Atom = nil, loc := #caller_location) -> ^Atom {return atom_new({kind=.Add, lhs=lhs, rhs=rhs},           from=from, loc=loc)}
@require_results atom_new_mul   :: proc (lhs, rhs: ^Atom,                  from: ^Atom = nil, loc := #caller_location) -> ^Atom {return atom_new({kind=.Mul, lhs=lhs, rhs=rhs},           from=from, loc=loc)}
@require_results atom_new_div   :: proc (lhs, rhs: ^Atom,                  from: ^Atom = nil, loc := #caller_location) -> ^Atom {return atom_new({kind=.Div, lhs=lhs, rhs=rhs},           from=from, loc=loc)}
@require_results atom_new_pow   :: proc (lhs, rhs: ^Atom,                  from: ^Atom = nil, loc := #caller_location) -> ^Atom {return atom_new({kind=.Pow, lhs=lhs, rhs=rhs},           from=from, loc=loc)}
@require_results atom_new_or    :: proc (lhs, rhs: ^Atom,                  from: ^Atom = nil, loc := #caller_location) -> ^Atom {return atom_new({kind=.Or,  lhs=lhs, rhs=rhs},           from=from, loc=loc)}
@require_results atom_new_and   :: proc (lhs, rhs: ^Atom,                  from: ^Atom = nil, loc := #caller_location) -> ^Atom {return atom_new({kind=.And, lhs=lhs, rhs=rhs},           from=from, loc=loc)}
@require_results atom_new_eq    :: proc (lhs, rhs: ^Atom,                  from: ^Atom = nil, loc := #caller_location) -> ^Atom {return atom_new({kind=.Eq,  lhs=lhs, rhs=rhs},           from=from, loc=loc)}
@require_results atom_new_get   :: proc (atom: ^Atom, name: string,        from: ^Atom = nil, loc := #caller_location) -> ^Atom {return atom_new({kind=.Get, get={atom=atom, name=name}}, from=from, loc=loc)}
@require_results atom_new_val   :: proc {atom_new_int, atom_new_float, atom_new_str}

@require_results atom_is_bin :: proc (atom: Atom) -> bool {return atom.kind in ATOM_BINARY_KINDS}
@require_results atom_is_num :: proc (atom: Atom) -> bool {return atom.kind in ATOM_NUM_KINDS}

@require_results atom_val_var_equals   :: proc (atom: Atom, var: string) -> bool {return atom.kind == .Var && atom.var == var}
@require_results atom_val_int_equals   :: proc (atom: Atom, val: int) -> bool    {return atom.kind == .Int && atom.int == val}
@require_results atom_val_float_equals :: proc (atom: Atom, val: f64) -> bool    {return atom.kind == .Float && atom.float == val}
@require_results atom_val_equals       :: proc {atom_val_var_equals, atom_val_int_equals, atom_val_float_equals}

@require_results
atom_num_equals_const :: proc (atom: Atom, $N: int) -> bool {
	#partial switch atom.kind {
	case .Int:   return atom.int   == N
	case .Float: return atom.float == f64(N)
	}
	return false
}
@require_results atom_num_equals_zero    :: proc (atom: Atom) -> bool {return atom_num_equals_const(atom, 0)}
@require_results atom_num_equals_one     :: proc (atom: Atom) -> bool {return atom_num_equals_const(atom, 1)}
@require_results atom_num_equals_neg_one :: proc (atom: Atom) -> bool {return atom_num_equals_const(atom, -1)}

@require_results
atom_add_if_possible :: proc (a, b: ^Atom, from: ^Atom = nil) -> (res: ^Atom, ok: bool)
{
	res, ok = distribute_over(.Add, a, b, from=from)
	if ok do return
	
	switch ([2]Atom_Kind{a.kind, b.kind}) {
	// 1 + 2  ->  3
	case {.Int,   .Int}:   return atom_new_val(a.int+b.int, from=from), true
	case {.Float, .Float}: return atom_new_val(a.float+b.float, from=from), true
	case {.Int,   .Float}: return atom_new_val(f64(a.int)+b.float, from=from), true
	case {.Float, .Int}:   return atom_new_val(a.float+f64(b.int), from=from), true
	// "foo" + "bar"  ->  "foobar"
	case {.Str,   .Str}:
		return atom_new_str(
			strings.concatenate({a.str, b.str}),
			from=from), true
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
			return atom, &atom_int_one
		}

		a_val, a_f := atom_val_factor(a)
		b_val, b_f := atom_val_factor(b)

		if atom_equals(a_val, b_val) {
			return atom_mul(
				a_val,
				atom_add(a_f, b_f),
				from=from,
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
			from=from,
		), true
	}

	res, ok = visit_b(a, b)
	if ok do return
	res, ok = visit_b(b, a)
	return
	
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
			if new_lhs, ok := atom_add_if_possible(b.lhs, a, b); ok {
				return atom_new_add(new_lhs, b.rhs, b), true
			}
			// (y + x) + x  ->  y + 2x
			if new_rhs, ok := atom_add_if_possible(b.rhs, a, b); ok {
				return atom_new_add(b.lhs, new_rhs, b), true
			}
		case .Mul, .Pow, .Var, .Str, .Or, .Eq, .And, .Get, .None:
		}
		return
	}
}
@require_results
atom_add :: proc (a, b: ^Atom, from: ^Atom = nil, loc := #caller_location) -> ^Atom {
	return atom_add_if_possible(a, b, from=from) or_else atom_new_add(a, b, from=from, loc=loc)
}
@require_results
atom_add_num :: proc (atom: ^Atom, f: f64, from: ^Atom = nil, loc := #caller_location) -> ^Atom {
	return atom_add(atom, atom_new_float(f, atom, loc), from=from, loc=loc)
}
@require_results
atom_sub :: proc (lhs, rhs: ^Atom, from: ^Atom = nil, loc := #caller_location) -> ^Atom {
	return atom_add(lhs, atom_neg(rhs, loc=loc), from=from, loc=loc)
}

@require_results
atom_mul_if_possible :: proc (a, b: ^Atom, from: ^Atom = nil) -> (res: ^Atom, ok: bool)
{
	res, ok = distribute_over(.Mul, a, b, from=from)
	if ok do return
	
	// 2 * 3  ->  6
	if a.kind == .Int && b.kind == .Int {
		return atom_new_val(a.int*b.int, from=from), true
	}
	else if a.kind == .Float && b.kind == .Float {
		return atom_new_val(a.float*b.float, from=from), true
	}
	else if a.kind == .Int && b.kind == .Float {
		return atom_new_val(f64(a.int)*b.float, from=from), true
	}
	else if a.kind == .Float && b.kind == .Int {
		return atom_new_val(a.float*f64(b.int), from=from), true
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

	res, ok = visit_b(a, b, from=from)
	if ok do return
	res, ok = visit_b(b, a, from=from)
	return

	visit_b :: proc (a, b: ^Atom, from: ^Atom) -> (res: ^Atom, ok: bool)
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
			return atom_new_pow(
				a.lhs,
				atom_add(a.rhs, &atom_int_one),
				from=from,
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
			case .Div: return atom_new_div(
				atom_mul(a.lhs, b),
				a.rhs,
				from=from,
			), true
			}
		}

		if a.kind == .Mul {
			// (x * y) * x  ->  x^2 * y
			if new_lhs, ok := atom_mul_if_possible(a.lhs, b, from=from); ok {
				return atom_new_mul(new_lhs, a.rhs, from=from), true
			}
			// (y * x) * x  ->  y * x^2
			if new_rhs, ok := atom_mul_if_possible(a.rhs, b, from=from); ok {
				return atom_new_mul(a.lhs, new_rhs, from=from), true
			}
		}

		if a.kind == .Div {
			// (x / y) * x  ->  x^2 / y
			if new_lhs, ok := atom_mul_if_possible(a.lhs, b, from=from); ok {
				return atom_new_div(new_lhs, a.rhs, from=from), true
			}
			// (y / x) * x  ->  y
			if atom_equals(a.rhs, b) {
				return atom_new(a.lhs^, from=from), true
			}
		}

		return
	}
}
@require_results
atom_mul :: proc (a, b: ^Atom, from: ^Atom = nil, loc := #caller_location) -> ^Atom {
	return atom_mul_if_possible(a, b, from=from) \
		or_else atom_new_mul(a, b, from=from, loc=loc)
}
@require_results
atom_mul_num :: proc (atom: ^Atom, f: f64, from: ^Atom = nil, loc := #caller_location) -> ^Atom {
	return atom_mul(atom, atom_new_float(f, loc=loc), from=from, loc=loc)
}
@require_results
atom_neg :: proc (atom: ^Atom, from: ^Atom = nil, loc := #caller_location) -> ^Atom {
	return atom_mul(atom, &atom_int_neg_one, from=from, loc=loc)
}

@require_results
atom_div_if_possible :: proc (dividened, divisor: ^Atom, from: ^Atom = nil) -> (res: ^Atom, ok: bool)
{
	res, ok = distribute_over(.Div, dividened, divisor, from=from)
	if ok do return
	
	// 0/x  ->  0
	if atom_num_equals_zero(dividened^) {
		return dividened, true
	}

	// x/0  ->  Inf
	if atom_num_equals_zero(divisor^) {
		if dividened.kind == .Int {
			if dividened.int < 0 {
				return atom_new_val(math.inf_f64(-1), from=from), true
			} else {
				return atom_new_val(math.inf_f64(1), from=from), true
			}
		} else if dividened.kind == .Float {
			if dividened.float < 0 {
				return atom_new_val(math.inf_f64(-1), from=from), true
			} else {
				return atom_new_val(math.inf_f64(1), from=from), true
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
			return atom_new_val(f64(dividened.int)/f64(divisor.int), from=from), true
		}
		return atom_new_val(dividened.int/divisor.int, from=from), true
	}
	else if divisor.kind == .Float && dividened.kind == .Float {
		return atom_new_val(dividened.float/divisor.float, from=from), true
	}
	else if divisor.kind == .Int && dividened.kind == .Float {
		return atom_new_val(dividened.float/f64(divisor.int), from=from), true
	}
	else if divisor.kind == .Float && dividened.kind == .Int {
		return atom_new_val(f64(dividened.int)/divisor.float, from=from), true
	}

	// x/x  ->  1
	if atom_equals(dividened, divisor) {
		return &atom_int_one, true
	}

	#partial switch dividened.kind {
	// x*2 / x  ->  1*2  ->  2
	case .Mul:
		if lhs, ok := atom_div_if_possible(dividened.lhs, divisor, from=from); ok {
			return atom_mul(lhs, dividened.rhs), true
		}
		if rhs, ok := atom_div_if_possible(dividened.rhs, divisor, from=from); ok {
			return atom_mul(dividened.lhs, rhs), true
		}
	// (a+b)/c  ->  a/c + b/c
	case .Add:
		return atom_add(
			atom_div_if_possible(dividened.lhs, divisor) or_break,
			atom_div_if_possible(dividened.rhs, divisor) or_break,
			from=from,
		), true
	case .Div:
		// (x/y)/x  ->  1/y
		if lhs, ok := atom_div_if_possible(dividened.lhs, divisor); ok {
			return atom_div(lhs, dividened.rhs, from=from), true
		}
		// (a/b)/c  ->  a/(b*c)
		if rhs, ok := atom_mul_if_possible(dividened.rhs, divisor); ok {
			return atom_div(dividened.lhs, rhs, from=from), true
		}
	case .Pow:
		// x^3 / x  ->  x^2
		if atom_equals(dividened.lhs, divisor) {
			if dividened.rhs.kind == .Int {
				return atom_pow(dividened.lhs, dividened.rhs.int-1, from=from), true
			} else if dividened.rhs.kind == .Float {
				return atom_pow(dividened.lhs, dividened.rhs.float-1, from=from), true
			}
		}
	}
	
	return dividened, false
}
@require_results
atom_div :: proc (dividened, divisor: ^Atom, from: ^Atom = nil, loc := #caller_location) -> ^Atom {
	return atom_div_if_possible(dividened, divisor, from=from) \
		or_else atom_new_div(dividened, divisor, from=from, loc=loc)
}
@require_results
atom_div_num :: proc (dividened: ^Atom, f: f64, from: ^Atom = nil, loc := #caller_location) -> ^Atom {
	switch f {
	case 0: return atom_new_val(math.inf_f64(-1), from=from, loc=loc)
	case 1: return atom_new_val(math.inf_f64(1), from=from, loc=loc)
	}
	return atom_div(dividened, atom_new_float(f, dividened, loc), from=from, loc=loc)
}
@require_results
atom_flip :: proc (atom: ^Atom, from: ^Atom = nil) -> ^Atom {
	#partial switch atom.kind {
	case .Div:   return atom_div(atom.rhs, atom.lhs)
	case .Int:   return atom_new_val(1/f64(atom.int), atom)
	case .Float: return atom_new_val(1/atom.float, atom)
	case:        return atom_div(&atom_int_one, atom)
	}
}

@require_results
has_dependencies :: proc (atom: Atom) -> bool {
	switch (atom.kind) {
	case .Var: return true
	case .Int, .Float, .Str, .None: return false
	case .Get: return true // ? Get always has dependencies
	case .Add, .Div, .Mul, .Pow, .Or, .And, .Eq:
		return has_dependencies(atom.lhs^) ||
		       has_dependencies(atom.rhs^)
	case: return false
	}
}
@require_results
has_dependency :: proc (atom: Atom, var: string) -> bool {
	switch (atom.kind) {
	case .Var: return atom.var == var
	case .Int, .Float, .Str, .None: return false
	case .Get: return atom.get.name == var || has_dependency(atom.get.atom^, var)
	case .Add, .Div, .Mul, .Pow, .Or, .And, .Eq:
		return has_dependency(atom.lhs^, var) ||
		       has_dependency(atom.rhs^, var)
	case: return false
	}
}
@require_results
has_dependency_other_than_var :: proc (atom: Atom, var: string) -> bool {
	switch (atom.kind) {
	case .Var: return atom.var != var
	case .Int, .Float, .Str, .None: return false
	case .Get: return atom.get.name != var && has_dependency_other_than_var(atom.get.atom^, var)
	case .Add, .Div, .Mul, .Pow, .Or, .And, .Eq:
		return has_dependency_other_than_var(atom.lhs^, var) ||
		       has_dependency_other_than_var(atom.rhs^, var)
	case: return false
	}
}

@require_results
atom_pow_if_possible :: proc (base, exponent: ^Atom, from: ^Atom = nil) -> (res: ^Atom, ok: bool) {

	res, ok = distribute_over(.Pow, base, exponent, from=from)
	if ok do return

	// 2^0  ->  1
	if atom_num_equals_zero(exponent^) {
		return atom_new_val(1, from=from), true
	}
	// 2^1  ->  2
	if atom_num_equals_one(exponent^) {
		return base, true
	}

	// 2^3  ->  8
	if exponent.kind == .Float && base.kind == .Float {
		return atom_new_val(math.pow(base.float, exponent.float), from=from), true
	}
	else if exponent.kind == .Int && base.kind == .Int {
		if res, ok := pow_int(base.int, exponent.int); ok {
			return atom_new_val(res, from=from), true
		}
		return atom_new_val(math.pow(f64(base.int), f64(exponent.int)), from=from), true
	}
	else if exponent.kind == .Int && base.kind == .Float {
		return atom_new_val(math.pow(base.float, f64(exponent.int)), from=from), true
	}
	else if exponent.kind == .Float && base.kind == .Int {
		return atom_new_val(math.pow(f64(base.int), exponent.float), from=from), true
	}

	return nil, false
}
@require_results
atom_pow_atom :: proc (base, exponent: ^Atom, from: ^Atom = nil) -> ^Atom {
	return atom_pow_if_possible(base, exponent) or_else
	       atom_new_pow(base, exponent, from=from)
}
@require_results
atom_pow_float :: proc (base: ^Atom, f: f64, from: ^Atom = nil) -> ^Atom {
	return atom_pow_atom(base, atom_new_val(f), from=from)
}
@require_results
atom_pow_int :: proc (base: ^Atom, f: int, from: ^Atom = nil) -> ^Atom {
	return atom_pow_atom(base, atom_new_val(f), from=from)
}
atom_pow :: proc {atom_pow_atom, atom_pow_float, atom_pow_int}

@require_results
atom_or_if_possible :: proc (lhs, rhs: ^Atom, from: ^Atom = nil) -> (^Atom, bool) {
	
	// if res, ok := distribute_over(.Or, .Or, lhs, rhs, from=from); ok {
	// 	return res, true
	// }

	// (a = b) | (a = c)  ->  a = (b | c)
	fold_eq: if lhs.kind == .Eq && rhs.kind == .Eq {
		a, b, c: ^Atom
		switch {
		case atom_equals(lhs.lhs, rhs.lhs): a, b, c = lhs.lhs, lhs.rhs, rhs.rhs
		case atom_equals(lhs.rhs, rhs.rhs): a, b, c = lhs.rhs, lhs.lhs, rhs.lhs
		case atom_equals(lhs.lhs, rhs.rhs): a, b, c = lhs.lhs, rhs.lhs, lhs.rhs
		case atom_equals(lhs.rhs, rhs.lhs): a, b, c = lhs.rhs, rhs.rhs, lhs.lhs
		}
		return atom_new_eq(a, atom_or(b, c), from=from), true
	}

	return {}, false
}

@require_results
atom_or :: proc (a, b: ^Atom, from: ^Atom = nil, loc := #caller_location) -> ^Atom {
	return atom_or_if_possible(a, b, from=from) \
		or_else atom_new_or(a, b, from=from, loc=loc)
}

@require_results
distribute_over :: proc (
	op: Atom_Kind, a, b: ^Atom, var: Maybe(string) = {}, from: ^Atom = nil,
) -> (res: ^Atom, ok: bool)
{
	// If 'a' is an or expression: (x | y) op b -> (x op b) | (y op b)
	for ab in ([][2]^Atom{{a, b}, {b, a}}) {
		a, b := ab[0], ab[1]

		if (a.kind == .And || a.kind == .Or) {
			lhs_res, lhs_ok := resolve_bin(op, a.lhs, b, var, from=from)
			rhs_res, rhs_ok := resolve_bin(op, a.rhs, b, var, from=from)

			if lhs_ok || rhs_ok {
				return atom_new_bin(a.kind,
					lhs_res if lhs_ok else atom_new_bin(op, a.lhs, b, from=from),
					rhs_res if rhs_ok else atom_new_bin(op, a.rhs, b, from=from),
					from=from,
				), true
			}
		}
	}
	return
}

atom_eq_if_possible :: proc (lhs, rhs: ^Atom, var_maybe: Maybe(string) = {}, from: ^Atom = nil) -> (res: ^Atom, updated: bool)
{
	// () = x   ->  ()
	// x  = ()  ->  ()
	// x  = x   ->  ()
	if lhs.kind == .None || rhs.kind == .None || atom_equals(lhs, rhs) {
		return atom_new_none(from=from), true
	}

	var := var_maybe.? or_return

	res, updated = distribute_over(.Eq, lhs, rhs, from=from)
	if updated do return

	res = from
	lhs, rhs := lhs, rhs

	/*
	move addends if they do(n't) depend on var
	*/
	lhs_has_var := has_dependency(lhs^, var)
	rhs_has_var := has_dependency(rhs^, var)
	if lhs_has_var || rhs_has_var {
		// 1+2 = x  ->  x = 1+2
		if !lhs_has_var && rhs_has_var {
			lhs, rhs = rhs, lhs
			updated = true
		}
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
	}

	move_addends :: proc (atom: ^Atom, dst: ^^Atom, var: string, cond: bool) -> (res: ^Atom, ok: bool)
	{
		if atom.kind == .Add {
			lhs, lhs_ok := move_addends(atom.lhs, dst, var, cond)
			rhs, rhs_ok := move_addends(atom.rhs, dst, var, cond)
			if lhs_ok || rhs_ok {
				return atom_add(lhs, rhs), true
			}
		} else if has_dependency(atom^, var) == cond && !atom_num_equals_zero(atom^) {
			dst^ = atom_sub(dst^, atom)
			return &atom_int_zero, true
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
		handle special case: a * b = 0  ->  a|b = 0
		*/
		if atom_num_equals_zero(rhs^) {
			lhs = atom_or(lhs.lhs, lhs.rhs)
			updated = true
		}

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
				return &atom_int_one, true
			}
			return atom, false
		}

	case .Add:
		/*
		extract var and divide rhs
		2a + 3ab = y  ->  a(2 + 3b) = y  ->  a = y / (2 + 3b)
		*/
		if div, ok := extract_var_if_possible(lhs, var); ok {
			lhs, rhs = atom_new_var(var), atom_div(rhs, div)
			updated = true
		}

		// has to completely remove the var from atom
		extract_var_if_possible :: proc (atom: ^Atom, var: string) -> (res: ^Atom, ok: bool) {
			res = atom

			switch atom.kind {
			case .Var:
				if atom.var == var {
					return &atom_int_one, true
				}
			case .Add:
				lhs := extract_var_if_possible(atom.lhs, var) or_return
				rhs := extract_var_if_possible(atom.rhs, var) or_return
				return atom_add(lhs, rhs), true
			case .Div:
				if !has_dependency(atom.rhs^, var) {
					lhs := extract_var_if_possible(atom.lhs, var) or_return
					return atom_div(lhs, atom.rhs), true
				}
			case .Mul:
				if lhs, ok := extract_var_if_possible(atom.lhs, var); ok {
					if !has_dependency(atom.rhs^, var) {
						return atom_mul(lhs, atom.rhs), true
					}
				} else if rhs, ok := extract_var_if_possible(atom.rhs, var); ok {
					return atom_mul(atom.lhs, rhs), true
				}
			case .Pow, .Int, .Float, .Str, .Or, .Eq, .And, .Get, .None:
				// skip
			}

			return
		}
	}

	if updated {
		res = atom_new_eq(lhs, rhs, from=from)
	}

	return
}

// Compares structurally
@require_results
atom_equals_val :: proc (a, b: Atom) -> bool
{
	if a.kind == b.kind {
		switch a.kind {
		case .None:  return true
		case .Int:   return a.int == b.int
		case .Float: return a.float == b.float
		case .Str:   return a.str == b.str
		case .Var:   return a.var == b.var
		case .Get:
			return a.get.name == b.get.name &&
				   atom_equals(a.get.atom, b.get.atom)
		case .Add, .Div, .Mul, .Pow, .Or, .Eq, .And:
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

// Attempts to resolve a binary operation, returning the result if simplification is possible
@require_results
resolve_bin :: proc(op: Atom_Kind, lhs, rhs: ^Atom, var: Maybe(string) = {}, from: ^Atom = nil) -> (^Atom, bool) {
	switch op {
	case .Add: return atom_add_if_possible(lhs, rhs, from=from)
	case .Mul: return atom_mul_if_possible(lhs, rhs, from=from)
	case .Div: return atom_div_if_possible(lhs, rhs, from=from)
	case .Pow: return atom_pow_if_possible(lhs, rhs, from=from)
	case .Or:  return atom_or_if_possible(lhs, rhs, from=from)
	case .Eq:  return atom_eq_if_possible(lhs, rhs, var, from=from)
	case .And:
		//   () & x  ->  x
		if lhs.kind == .None {
			return atom_new(rhs^, from=from), true
		} // x & ()  ->  x
		else if rhs.kind == .None {
			return atom_new(lhs^, from=from), true
		} // x & x  ->  x
		else if atom_equals(lhs, rhs) {
			return atom_new(lhs^, from=from), true
		}
	case .Get, .None, .Int, .Float, .Str, .Var:
		unreachable()
	}
	return nil, false
}

// Specialized version for Eq operations that need a var parameter
@require_results
resolve_bin_eq :: proc(lhs, rhs: ^Atom, var: string, from: ^Atom = nil) -> (result: ^Atom, simplified: bool) {
	return atom_eq_if_possible(lhs, rhs, var, from)
}

_unused_updated: bool

fold_atom :: proc (atom: ^^Atom, constrs: ^Constraints, var: string) -> (updated: bool)
{
	switch atom^.kind {
	case .Add, .Mul, .Div, .Pow, .Eq, .Or, .And:
		lhs, rhs := atom^.lhs, atom^.rhs
		lhs_updated := fold_atom(&lhs, constrs, var)
		rhs_updated := fold_atom(&rhs, constrs, var)

		res, bin_updated := resolve_bin(atom^.kind, lhs, rhs, var, from=atom^)

		if bin_updated {
			// If the bin was updated, then we can use it
			atom^ = res
			updated = true
		} else if lhs_updated || rhs_updated {
			// If lhs or rhs were updated, but not the bin itself
			// then we still need to update the atom
			atom^ = atom_new_bin(atom^.kind, lhs, rhs, atom^)
			updated = true
		}
	case .Get:
		// Try getting the value of the var using recursive search
		if res, ok := resolve_get(atom^, atom^.get.atom, from=atom^); ok {
			atom^ = res
			updated = true
		} else {
			// Try folding the inner atom
			get_atom := atom^.get.atom
			if fold_atom(&get_atom, constrs, atom^.get.name) {
				atom^ = atom_new_get(get_atom, atom^.get.name, from=atom^)
				updated = true
			}
		}

		resolve_get :: proc (get, atom, from: ^Atom) -> (result: ^Atom, changed: bool) {
			#partial switch atom.kind {
			// ((a = x) & y).a  ->  x & (y).a
			// ((a = x) | y).a  ->  x | (y).a
			case .And, .Or:
				lhs, lhs_ok := resolve_get(get, atom.lhs, from=atom.lhs)
				rhs, rhs_ok := resolve_get(get, atom.rhs, from=atom.rhs)
				if lhs_ok || rhs_ok {
					if !lhs_ok {
						lhs = atom_new_get(lhs, get.get.name, from=lhs)
					}
					if !rhs_ok {
						rhs = atom_new_get(rhs, get.get.name, from=rhs)
					}
					return atom_new_bin(atom.kind, lhs, rhs, from=get), true
				}
			// (a = x).a  ->  x
			case .Eq:
				if atom_val_equals(atom.lhs^, get.get.name) do return atom.rhs, true
				if atom_val_equals(atom.rhs^, get.get.name) do return atom.lhs, true
				// (a|b = x).a  ->  x | (b = x).a
				or_case: {
					a, b, x: ^Atom
					switch {
					case atom.lhs.kind == .Or && atom_val_equals(atom.lhs.lhs^, get.get.name):
						a, b, x = atom.lhs.lhs, atom.lhs.rhs, atom.rhs
					case atom.lhs.kind == .Or && atom_val_equals(atom.lhs.rhs^, get.get.name):
						a, b, x = atom.lhs.rhs, atom.lhs.lhs, atom.rhs
					case atom.rhs.kind == .Or && atom_val_equals(atom.rhs.lhs^, get.get.name):
						a, b, x = atom.rhs.lhs, atom.rhs.rhs, atom.lhs
					case atom.rhs.kind == .Or && atom_val_equals(atom.rhs.rhs^, get.get.name):
						a, b, x = atom.rhs.rhs, atom.rhs.lhs, atom.lhs
					case: break or_case
					}
					return atom_new_or(
						x,
						atom_new_get(
							atom_new_eq(b, x),
							get.get.name),
						from=from,
					), true
				}
			}
			// (a * 2 = x).a (not resolved)
			if has_dependency(atom^, get.get.name) {
				return atom, false
			}
			// (x = 2).a  ->  (x = 2)
			return atom_new(atom^, from=from), true
		}
	case .Var:
		// Try substituting the var from constraints
		if var == atom^.var do break
		constr := constrs.vars[atom^.var] or_break

		if res, ok := find_eq(constr, atom^.var, var); ok {
			atom^ = atom_new(res^, from=atom^)
			updated = true
		}

		// for x: (x = 3) & (y = 2)  ->  3
		find_eq :: proc (atom: ^Atom, var, constr_var: string) -> (res: ^Atom, ok: bool)
		{
			#partial switch atom.kind {
			case .Eq:
				if atom_val_equals(atom.lhs^, var) {
					res, ok = find_substitution(atom.rhs, var, constr_var)
					if ok do break
				}
				if atom_val_equals(atom.rhs^, var) {
					res, ok = find_substitution(atom.lhs, var, constr_var)
				}
			case .And:
				res, ok = find_eq(atom.lhs, var, constr_var)
				if ok do break
				res, ok = find_eq(atom.rhs, var, constr_var)
			}
			return
		}
		
		// for x: 3 & (x + 2) -> 3
		find_substitution :: proc (atom: ^Atom, var, constr_var: string) -> (res: ^Atom, ok: bool)
		{
			if atom.kind == .And {
				res, ok = find_substitution(atom.lhs, var, constr_var)
				if ok do return
				res, ok = find_substitution(atom.rhs, var, constr_var)
				return
			}
			return atom, atom.kind != .None &&
			       !has_dependency(atom^, var) &&
			       !has_dependency(atom^, constr_var)
		}
	case .Int, .Float, .Str, .None:
		// constants and vars are not foldable
	}

	return
}

atom_from_expr :: proc (expr: Expr) -> (a: ^Atom)
{
	switch v in expr {
	case ^Expr_Single:

		#partial switch v.token.kind {
		case .Float:
			value, _ := expr_single_float_value(v^)
			// TODO: handle errors
			return atom_new_float(value)
		case .Int:
			value, _ := expr_single_int_value(v^)
			// TODO: handle errors
			return atom_new_int(value)
		case .Str:
			value, _ := expr_single_string_value(v^)
			// TODO: handle errors
			return atom_new_str(value)
		case .Ident:
			name := expr_single_ident_value(v^)
			a = atom_new_var(name)
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
		case .And, .EOL:
		           a.kind = .And
		case: unreachable()
		}
	}

	return
}
	
@require_results
constraints_from_expr :: proc (expr: Expr, allocator := context.allocator) -> (constrs: Constraints)
{
	context.allocator = allocator

	constrs.vars  = make(map[string]^Atom, allocator)
	constrs.order = make([dynamic]string, allocator)

	atom := atom_from_expr(expr)

	/*
		Add constr for each var in atom
		and exclude vars that are already in constrs
	*/
	_visit(atom, atom, &constrs)
	_visit :: proc (atom: ^Atom, root_atom: ^Atom, constrs: ^Constraints)
	{
		switch atom.kind {
		case .Int, .Float, .Str:
			// Do nothing for constants
			return
		case .Var:
			if atom.var not_in constrs.vars {
				append(&constrs.order, atom.var)
				constrs.vars[atom.var] = atom_new_eq(
					atom_new_var(atom.var),
					atom_new_get(root_atom, atom.var))
			}
		case .None: unimplemented("none expressions are not supported")
		case .Get:
			unimplemented("get expressions are not supported")
		case .Add, .Mul, .Div, .Pow, .Eq, .Or, .And:
			_visit(atom.lhs, root_atom, constrs)
			_visit(atom.rhs, root_atom, constrs)
		}
	}

	return
}

solve :: proc (constrs: ^Constraints, allocator := context.allocator)
{
	context.allocator = allocator

	solve_loop: for {
		updated: bool

		for var in constrs.order {
			fold_atom(&constrs.vars[var], constrs, var) or_continue
			updated = true
		}

		if updated do continue

		break
	}
}

try_finding_polynomial_solution :: proc (atom: ^^Atom, var: string, constrs: ^Constraints, updated: ^bool)
{
	if atom^.kind != .Eq ||
	   (atom_val_equals(atom^.lhs^, var) && !has_dependencies(atom^.rhs^)) ||
	   has_dependency_other_than_var(atom^^, var) {
		return
	}

	/*
	move right to the left to have a single atom equals 0
	x^2 + x = 12  ->  x^2 + x + -12 = 0
	*/
	sub := atom_sub(atom^.lhs, atom^.rhs)
	fold_atom(&sub, constrs, var)

	poly, ok := polynomial_from_atom(sub^, var, allocator = context.temp_allocator)
	if !ok do return

	root, found := find_polynomial_root(poly)
	if found {
		atom^ = atom_new_eq(
			atom_new_var(var, sub^.lhs),
			atom_new_val(root, sub^.rhs),
			from=atom^)
		updated^ = true
	}
}
