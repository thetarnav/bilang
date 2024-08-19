package bilang

import "base:runtime"

import "core:math"
import "core:strings"

/*
atom pointers can be repeated, (same pointers in multiple places)
so the value of atom behind a pointer must stay the same,
when changing the value a new atom must be allocated and pointer changed
*/
Atom :: struct {
	kind:     Atom_Kind,
	using _: struct #raw_union {
		using bin: struct {lhs, rhs: ^Atom},
		num: f64,
		var: string,
	},
}

Atom_Kind :: enum {
	Num,
	Var,
	Add,
	Mul,
	Div,
	Pow,
}

Constraint :: struct {
	var:      ^Atom,
	lhs, rhs: ^Atom,
}

@require_results
atom_new :: proc (atom: Atom, loc := #caller_location) -> ^Atom {
	a, err := new(Atom, loc=loc)
	assertf(err == nil, "atom_new error: %v", err, loc=loc)
	a^ = atom
	return a
}

@require_results
atom_num :: proc (num: f64, loc := #caller_location) -> ^Atom {
	return atom_new({
		kind = .Num,
		num  = num,
	}, loc)
}
@require_results
atom_var :: proc (var: string, loc := #caller_location) -> ^Atom {
	return atom_new({
		kind = .Var,
		var  = var,
	}, loc)
}
@require_results
atom_binary :: proc (kind: Atom_Kind, lhs, rhs: ^Atom, loc := #caller_location) -> ^Atom {
	return atom_new({
		kind = kind,
		lhs  = lhs,
		rhs  = rhs,
	}, loc)
}

@require_results
is_var :: proc (atom: Atom, var: string) -> bool {
	return atom.kind == .Var && atom.var == var
}
@require_results
is_binary :: proc (kind: Atom_Kind) -> bool {
	#partial switch kind {
	case .Add, .Div, .Mul, .Pow: return true
	case:                        return false
	}
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

atom_get_mul_val_and_factor :: proc (atom: ^Atom) -> (val: ^Atom, f: f64)
{
	if atom.kind == .Mul {
		if atom.lhs.kind == .Num {
			return atom.rhs, atom.lhs.num
		}
		if atom.rhs.kind == .Num {
			return atom.lhs, atom.rhs.num
		}
	}
	return atom, 1
}

/* 
 x * x   ->  x, 1, 1
2x * x   ->  x, 2, 1
 x * 2x  ->  x, 1, 2
2x * 2x  ->  x, 2, 2
*/
atoms_get_mul_val_and_factors :: proc (a, b: ^Atom) -> (val: ^Atom, a_f, b_f: f64, ok: bool)
{
	a_val, a_num := atom_get_mul_val_and_factor(a)
	b_val, b_num := atom_get_mul_val_and_factor(b)
	atom_equals(a_val, b_val) or_return
	return a_val, a_num, b_num, true
}

atom_num_zero := Atom{kind=.Num, num=0}
atom_num_one  := Atom{kind=.Num, num=1}

@require_results
atom_add_if_possible :: proc (a, b: ^Atom) -> (sum: ^Atom, ok: bool)
{
	return visit_a_b(a, b)

	visit_a_b :: proc (a, b: ^Atom) -> (res: ^Atom, ok: bool)
	{
		// 1 + 2  ->  3
		if a.kind == .Num && b.kind == .Num {
			return atom_num(a.num+b.num), true
		}

		// 2x + 3x  ->  5x
		if val, a_f, b_f, ok := atoms_get_mul_val_and_factors(a, b); ok {
			return atom_mul_num(val, a_f+b_f), true
		}

		// x + x  ->  2x
		if atom_equals(a, b) {
			return atom_mul_num(a, 2), true
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
		// x + 0  ->  x
		if b.kind == .Num && b.num == 0 {
			return a, true
		}

		if a.kind == .Add {
			// (x + y) + x  ->  2x + y
			if new_lhs, ok := visit_a_b(a.lhs, b); ok {
				return atom_binary(.Add, new_lhs, a.rhs), true
			}
			// (y + x) + x  ->  y + 2x
			if new_rhs, ok := visit_a_b(a.rhs, b); ok {
				return atom_binary(.Add, a.lhs, new_rhs), true
			}
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
	return atom_add(atom, atom_num(f, loc), loc)
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
		// 2x * 3x  ->  6 * x^2
		if val, a_f, b_f, ok := atoms_get_mul_val_and_factors(a, b); ok {
			return atom_binary(.Mul,
				atom_num(a_f * b_f),
				atom_pow_num(val, 2),
			), true
		}

		// (a + b) * (c + d) -> a*c + a*d + b*c + b*d
		if a.kind == .Add && b.kind == .Add {
			return atom_binary(.Add,
				atom_binary(.Add,
					atom_mul(a.lhs, b.lhs),
					atom_mul(a.lhs, b.rhs),
				),
				atom_binary(.Add,
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
		// x^2 * x  ->  x^3
		// ? should other exponents besided num be allowed?
		if a.kind == .Pow && a.rhs.kind == .Num && atom_equals(a.lhs, b) {
			return atom_binary(.Pow,
				a.lhs,
				atom_num(a.rhs.num+1),
			), true
		}

		if b.kind == .Num {
			switch b.num {
			// x * 0  ->  0
			case 0: return b, true
			// x * 1  ->  x
			case 1: return a, true
			}

			// ? 	All these beyond num are sus
			// ? probably need to use the atom_foo_if_possible procs here
			// ? and take it out from num rhs
			// ? 	Also usually you try to get the common factor out of an expression
			// ? not to it. eg `4a + 4b  ->  4(a + b)` (2 ops < 3 ops)
			#partial switch a.kind {
			// 2 * 3  ->  6
			case .Num: return atom_num(a.num*b.num), true
			// (a + b) * 4  ->  4a + 4b
			case .Add: return atom_binary(.Add,
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
	return atom_mul(atom, atom_num(f, loc), loc)
}
@require_results
atom_neg :: proc (atom: ^Atom, loc := #caller_location) -> ^Atom {
	return atom_mul_num(atom, -1, loc)
}

@require_results
atom_div_if_possible :: proc (dividened, divisor: ^Atom) -> (quotient: ^Atom, ok: bool)
{
	// 0/x  ->  0
	if dividened.kind == .Num && dividened.num == 0 {
		return &atom_num_zero, true
	}

	if divisor.kind == .Num do switch divisor.num {
	// x/0  ->  X_X
	case 0: return dividened, false
	// x/1  ->  x
	case 1: return dividened, true
	}

	// 6/3  ->  2
	if divisor.kind == .Num && dividened.kind == .Num {
		return atom_num(dividened.num/divisor.num), true
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
	case .Add: return atom_add(
		atom_div(dividened.lhs, divisor),
		atom_div(dividened.rhs, divisor),
	), true
	// (a/b)/c  ->  a/(b*c)
	case .Div: return atom_div(
		dividened.lhs,
		atom_mul(dividened.rhs, divisor),
	), true
	}
	
	return dividened, false
}
@require_results
atom_div :: proc (dividened, divisor: ^Atom) -> ^Atom
{
	quotient, ok := atom_div_if_possible(dividened, divisor)
	if !ok {
		assert(divisor.kind != .Num || divisor.num != 0, "division by zero")
		quotient = atom_binary(.Div, dividened, divisor)
	}
	return quotient
}
@require_results
atom_div_num :: proc (dividened: ^Atom, f: f64) -> ^Atom {
	return atom_div(dividened, atom_num(f))
}
@require_results
atom_flip :: proc (atom: ^Atom) -> ^Atom {
	return atom_div(atom.rhs, atom.lhs)
}

@require_results
is_dividable_by_var :: proc (atom: Atom, var: string) -> bool
{
	switch atom.kind {
	case .Num, .Pow:
		return false
	case .Var:
		return atom.var == var
	case .Add:
		return is_dividable_by_var(atom.lhs^, var) &&
		       is_dividable_by_var(atom.rhs^, var)
	case .Mul:
		return is_dividable_by_var(atom.lhs^, var) ||
		       is_dividable_by_var(atom.rhs^, var)
	case .Div:
		return is_dividable_by_var(atom.lhs^, var) &&
		       !has_dependency(atom.rhs^, var)
	}
	return false
}

@require_results
has_dependencies :: proc (atom: Atom) -> bool
{
	#partial switch atom.kind {
	case .Num: return false
	case .Var: return true
	case:      return has_dependencies(atom.lhs^) ||
	                  has_dependencies(atom.rhs^)
	}
}
@require_results
has_dependency :: proc (atom: Atom, var: string) -> bool
{
	#partial switch atom.kind {
	case .Num: return false
	case .Var: return atom.var == var
	case:      return has_dependency(atom.lhs^, var) ||
	                  has_dependency(atom.rhs^, var)
	}
}
@require_results
has_dependency_other_than_var :: proc (atom: Atom, var: string) -> bool
{
	#partial switch atom.kind {
	case .Num: return false
	case .Var: return atom.var != var
	case:      return has_dependency_other_than_var(atom.lhs^, var) ||
	                  has_dependency_other_than_var(atom.rhs^, var)
	}
}

@require_results
atom_pow_if_possible :: proc (base, exponent: ^Atom) -> (pow: ^Atom, ok: bool) {
	// 2^3  ->  8
	if base.kind == .Num && exponent.kind == .Num {
		return atom_num(math.pow(base.num, exponent.num)), true
	}

	return
}
@require_results
atom_pow :: proc (base, exponent: ^Atom) -> ^Atom {
	return atom_pow_if_possible(base, exponent) or_else
	       atom_binary(.Pow, base, exponent)
}
@require_results
atom_pow_num :: proc (base: ^Atom, f: f64) -> ^Atom {
	return atom_pow(base, atom_num(f))
}

// Compares structurally
@require_results
atom_equals_val :: proc (a, b: Atom) -> bool
{
	if a.kind != b.kind {
		return false
	}

	#partial switch a.kind {
	case .Num: return a.num == b.num
	case .Var: return a.var == b.var
	case:      return atom_equals_ptr(a.lhs, b.lhs) &&
	                  atom_equals_ptr(a.rhs, b.rhs)
	}
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
		if !is_binary(atom.kind) {
			return atom, false
		}

		fold_atom(&atom.lhs, &updated)
		fold_atom(&atom.rhs, &updated)

		op_updated: bool
		#partial switch atom.kind {
		case .Add: res, op_updated = atom_add_if_possible(atom.lhs, atom.rhs)
		case .Mul: res, op_updated = atom_mul_if_possible(atom.lhs, atom.rhs)
		case .Div: res, op_updated = atom_div_if_possible(atom.lhs, atom.rhs)
		case .Pow: res, op_updated = atom_pow_if_possible(atom.lhs, atom.rhs)
		}

		return res, updated || op_updated
	}
}

try_substituting_var :: proc (atom: ^Atom, var: string, value: ^Atom) -> (res: ^Atom, ok: bool)
{
	if is_var(atom^, var) {
		return value, true
	}

	if is_binary(atom.kind) {
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
	return is_var(constr.lhs^, constr.var.var) && !has_dependencies(constr.rhs^)
}

// Compares vars by value if they do not contradict
// constraint_contadicts :: proc (a, b: Constraint) -> bool
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
		case ^Expr_Ident:
			a = atom_var(v.name)
			
			for c in all_constraints[decl_start:] {
				if c.var.var == v.name do return a
			}
			// var not in decl constraints
			append(all_constraints, Constraint{var = a})

		case ^Expr_Number:
			return atom_num(v.value)
			
		case ^Expr_Unary:
			a = atom_from_expr(v.rhs, all_constraints, decl_start)
			if v.op == .Neg {
				a = atom_neg(a)
			}

		case ^Expr_Binary:
			a = atom_new({
				lhs = atom_from_expr(v.lhs, all_constraints, decl_start),
				rhs = atom_from_expr(v.rhs, all_constraints, decl_start),
			})
			
			switch v.op {
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
			output := contraints_to_string(data.constrs[:], {highlight=true}, context.temp_allocator)
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
					if constr_i == constr2_i && constr.var == constr2.var {
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

	move_addends :: proc (atom: ^Atom, dst: ^^Atom, var: string, cond: bool) -> (res: ^Atom, updated: bool)
	{
		if atom.kind == .Add {
			lhs, updated_lhs := move_addends(atom.lhs, dst, var, cond)
			rhs, updated_rhs := move_addends(atom.rhs, dst, var, cond)

			if updated_lhs || updated_rhs {
				return atom_add(lhs, rhs), true
			}
		}
		else if has_dependency(atom^, var) == cond {
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

	/*
	extract var and divide rhs
	2a + 3ab = y  ->  a(2 + 3b) = y  ->  a = y / (2 + 3b)
	*/
	if constr.lhs.kind != .Var && is_dividable_by_var(constr.lhs^, constr.var.var) {
		constr.rhs = atom_div(
			constr.rhs,
			atom_div(constr.lhs, constr.var),
		)
		constr.lhs = constr.var

		log_debug("extracting var")
		updated^ = true
	}


	switch constr.lhs.kind {
	case .Num, .Var, .Mul, .Add:
		// ignore

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
			constr.rhs = atom_pow(
				constr.rhs,
				atom_flip(constr.lhs.rhs),
			)
			constr.lhs = constr.lhs.lhs

			log_debug("moved exponent to rhs")
			updated^ = true
		}
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
	if !ok {
		return
	}

	root, found := find_polynomial_root(poly)
	if found {
		constr.lhs = atom_var(constr.var.var)
		constr.rhs = atom_num(root)
		updated^   = true
		log_debug("found polynomial solution")
	}
}
