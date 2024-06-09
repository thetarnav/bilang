package bilang

import "core:log"

@(disabled=!ODIN_DEBUG)
log_debug :: #force_inline proc (args: ..any, sep := " ", location := #caller_location)
{
	log.debug(..args, sep=sep, location=location)
}
@(disabled=!ODIN_DEBUG)
log_debugf :: #force_inline proc (fmt_str: string, args: ..any, location := #caller_location)
{
	log.debugf(fmt_str, ..args, location=location)
}
@(disabled=!ODIN_DEBUG)
log_debug_update :: proc (constrs: []Constraint, title := "updated", alloc := context.temp_allocator, location := #caller_location)
{
	output, err := contraints_to_string(constrs)

	if err != nil {
		log_debugf("%s: failed to print constraints: %v", title, err, location=location)
	} else {
		log_debugf("%s:\n%s", title, output, location=location)
	}
}


Atom :: union #no_nil {
	Atom_Num,
	Atom_Var,
	Atom_Add,
	Atom_Mul,
	Atom_Div,
}

Fraction :: struct {
	num: f64,
	den: f64,
}

Atom_Num :: struct {
	using f: Fraction,
}

Atom_Var :: struct {
	name: string,
	f: Fraction,
}

Atom_Add :: struct {
	addends: [dynamic]Atom,
}

Atom_Mul :: struct {
	factors: [dynamic]Atom,
}

Atom_Div :: struct {
	dividend: ^Atom,
	divisor : ^Atom,
}

Constraint :: struct {
	var: string,
	lhs: ^Atom,
	rhs: ^Atom,
}

FRACTION_IDENTITY: Fraction : {1, 1}
FRACTION_ZERO    : Fraction : {0, 1}


@(require_results)
new_atom :: proc (atom: Atom) -> ^Atom
{
	a, err := new(Atom)
	if err != nil do log.error("new_atom failed", err)
	a ^= atom
	return a
}

atom_num_make :: proc (num: f64, den: f64 = 1) -> Atom_Num
{
	return Atom_Num{{num, den}}
}

atom_var_make :: proc (name: string, f: Fraction = FRACTION_IDENTITY) -> Atom_Var
{
	return Atom_Var{name, f}
}

atom_add_make :: proc (lhs: Atom, rhs: Atom) -> Atom_Add #no_bounds_check
{
	add: Atom_Add
	add.addends = make([dynamic]Atom, 2, 12)
	add.addends[0] = lhs
	add.addends[1] = rhs
	return add
}

atom_mul_make :: proc (lhs: Atom, rhs: Atom) -> Atom_Mul #no_bounds_check
{
	mul: Atom_Mul
	mul.factors = make([dynamic]Atom, 2, 12)
	mul.factors[0] = lhs
	mul.factors[1] = rhs
	return mul
}

atom_div_make :: proc (dividend: Atom, divisor: Atom) -> Atom_Div
{
	div: Atom_Div
	div.dividend = new_atom(dividend)
	div.divisor  = new_atom(divisor)
	return div
}

atom_from_expr :: proc (
	expr:             Expr,
	all_constraints:  ^[dynamic]Constraint,
	decl_start:       int,                  // index into all_constraints
) -> Atom
{
	switch v in expr {
	case ^Expr_Ident:
		var := atom_var_make(v.name)

		// var already in decl constraints
		for c in all_constraints[decl_start:] {
			if c.var == v.name do return var
		}
		// var not in decl constraints
		append(all_constraints, Constraint{var = v.name})

		return var

	case ^Expr_Number:
		return atom_num_make(v.value)
	case ^Expr_Binary:
		lhs := atom_from_expr(v.lhs, all_constraints, decl_start)
		rhs := atom_from_expr(v.rhs, all_constraints, decl_start)
		
		switch v.op {
		case .Add:
			return atom_add_make(lhs, rhs)
		case .Sub:
			atom_neg(&rhs)
			return atom_add_make(lhs, rhs)
		case .Mul:
			return atom_mul_make(lhs, rhs)
		case .Div:
			return atom_div_make(lhs, rhs)
		}
	case ^Expr_Unary:
		switch v.op {
		case .Neg:
			rhs := atom_from_expr(v.rhs, all_constraints, decl_start)
			atom_neg(&rhs)
			return rhs
		case .Pos:
			return atom_from_expr(v.rhs, all_constraints, decl_start)
		}
	}

	return Atom_Num{FRACTION_ZERO}
}

@(require_results)
solve :: proc (decls: []Decl, allocator := context.allocator) -> []Constraint
{
	context.allocator = allocator

	constrs := make([dynamic]Constraint, 0, 16)
	defer shrink(&constrs)

	for decl in decls {
		decl_start := len(constrs)

		lhs := atom_from_expr(decl.lhs, &constrs, decl_start)
		rhs := atom_from_expr(decl.rhs, &constrs, decl_start)
		
		for &constr in constrs[decl_start:] {
			constr.lhs  = new_atom(atom_copy(lhs))
			constr.rhs  = new_atom(atom_copy(rhs))
		}

		// TODO: free lhs and rhs
	}

	for {
		updated: bool
		for _, i in constrs {
			walk_constraint(i, constrs[:], &updated)
		}
		if !updated do break
	}

	return constrs[:]
}

walk_constraint :: proc (constr_i: int, constrs: []Constraint, updated: ^bool)
{
	#no_bounds_check constr := &constrs[constr_i]
	
	walk_atom(constr.lhs, constr_i, constrs, updated)
	walk_atom(constr.rhs, constr_i, constrs, updated)


	/*
	Keep the var on the left side
	*/
	if !has_dependency(constr.lhs^, constr.var) && has_dependency(constr.rhs^, constr.var)
	{
		constr.lhs, constr.rhs = constr.rhs, constr.lhs

		log_debug_update(constrs, "swapping sides")
		updated ^= true
	}


	switch &lhs in constr.lhs {
	case Atom_Num:
		// ignore

	case Atom_Var:
		if lhs.f == FRACTION_IDENTITY do break
		
		atom_div(constr.rhs, lhs.f)
		lhs.f = FRACTION_IDENTITY
	
		log_debug_update(constrs, "dividing rhs by lhs var")
		updated ^= true
		
	case Atom_Add:
		#reverse for &a, i in lhs.addends {
			if has_dependency(a, constr.var) do continue

			atom_sub_by_atom(constr.rhs, a)
			unordered_remove(&lhs.addends, i)

			log_debug_update(constrs, "substracting addend lhs")
			updated ^= true
		}

	case Atom_Mul:
		#reverse for &a, i in lhs.factors {
			if has_dependency(a, constr.var) do continue

			atom_div_by_atom(constr.rhs, a)
			unordered_remove(&lhs.factors, i)

			log_debug_update(constrs, "dividing by factor lhs")
			updated ^= true
		}

	case Atom_Div:
		atom_mul_by_atom(constr.rhs, lhs.divisor^)
		constr.lhs = lhs.dividend
		log_debug_update(constrs, "moved lhs div to rhs")
		updated ^= true
	}

	if updated^ {
		walk_atom(constr.lhs, constr_i, constrs, updated)
		walk_atom(constr.rhs, constr_i, constrs, updated)
	}

	switch &rhs in constr.rhs {
	case Atom_Num:
		// ignore

	case Atom_Var:
		// ignore

	case Atom_Add:
		#reverse for a, i in rhs.addends {
			if !has_dependency(a, constr.var) do continue

			atom_sub_by_atom(constr.lhs, a)
			unordered_remove(&rhs.addends, i)

			log_debug_update(constrs, "substracting addend rhs")
			updated ^= true
		}

	case Atom_Mul:
		// ignore

	case Atom_Div:
		// ignore
	}
}

walk_atom :: proc (atom: ^Atom, constr_i: int, constrs: []Constraint, updated: ^bool)
{
	switch &a in atom {
	case Atom_Num:
		// a.num /= a.den

	case Atom_Var:
		constr := &constrs[constr_i]

		if a.name == constr.var do break

		for c, i in constrs {
			var, is_var := c.lhs.(Atom_Var)

			if !is_var ||
			   i == constr_i ||
			   a.name != var.name ||
			   c.rhs == constr.rhs ||
			   has_dependencies(c.rhs^) {
				continue
			}

			copy := atom_copy(c.rhs^)

			atom_div(&copy, var.f)
			atom_mul(&copy, a.f)
			atom ^= copy

			log_debug_update(constrs, "substituting var")
			updated ^= true
		}
		
	case Atom_Add:
		#reverse for &addend, i in a.addends {

			walk_atom(&addend, constr_i, constrs, updated)

			switch &v in addend {
			case Atom_Num:
				// fold adding zeros
				if v.num == 0 {
					unordered_remove(&a.addends, i)
					log_debug_update(constrs, "removing zero")
					updated ^= true
					break
				}

				for &addend2, j in a.addends {
					if j == i do continue

					switch &v2 in addend2 {
					case Atom_Num:
						v2.f = fraction_sum(v2.f, v.f)
					case Atom_Add:
						append(&v2.addends, addend)
					case Atom_Div:
						/*
					    (a / b) + 3/2 -> (2a + 3b) / 2b
						*/
						rhs := atom_copy(v2.divisor^)
						atom_mul(v2.dividend, {v.f.den, 1})
						atom_mul(&rhs,        {v.f.num, 1})
						atom_mul(v2.divisor,  {v.f.den, 1})
						v2.dividend ^= atom_add_make(v2.dividend^, rhs)
					case Atom_Var, Atom_Mul:
						continue
					}

					unordered_remove(&a.addends, i)

					log_debug_update(constrs, "folding adding num")
					updated ^= true
					break
				}

			case Atom_Var:
				// fold adding same vars
				for &addend2, j in a.addends {
					if j == i do continue

					var2 := (&addend2.(Atom_Var)) or_continue
					(var2.name == v.name) or_continue
					
					var2.f = fraction_sum(var2.f, v.f)
					unordered_remove(&a.addends, i)

					log_debug_update(constrs, "folding adding same vars")
					updated ^= true
				}

			case Atom_Add:
				// fold adding additions
				for &addend2, j in a.addends {
					if j == i do continue

					add2 := (&addend2.(Atom_Add)) or_continue

					append(&add2.addends, ..v.addends[:])
					unordered_remove(&a.addends, i)

					log_debug_update(constrs, "folding adding additions")
					updated ^= true
				}

			case Atom_Mul, Atom_Div:
				// TODO
			}
		}

		// len = 1
		if len(a.addends) == 1 {
			atom ^= a.addends[0]
			log_debug_update(constrs, "single addend")
			updated ^= true	
		}

	case Atom_Mul:
		#reverse for &factor, i in a.factors {

			walk_atom(&factor, constr_i, constrs, updated)

			switch v in factor {
			case Atom_Num:
				
				// fold multiplying zeros
				if v.num == 0 {
					atom ^= Atom_Num{FRACTION_ZERO}
					log_debug_update(constrs, "multiply by zero")
					updated ^= true
					return // atom is now a num
				}

				// fold multiplying ones
				if v.f == FRACTION_IDENTITY && len(a.factors) > 1 {
					unordered_remove(&a.factors, i)
	
					log_debug_update(constrs, "removing one")
					updated ^= true
					break
				}

				// fold multiplying num
				for &factor2, j in a.factors {
					if j == i do continue

					switch &v2 in factor2 {
					case Atom_Num:
						v2.f = fraction_product(v2.f, v.f)
					case Atom_Var:
						v2.f = fraction_product(v2.f, v.f)
					case Atom_Add:
						for &addend in v2.addends {
							atom_mul(&addend, v.f)
						}
					case Atom_Mul, Atom_Div:
						continue
					}

					unordered_remove(&a.factors, i)
					log_debug_update(constrs, "fold multiplying num")
					updated ^= true
					break
				}
			
			case Atom_Var:
				
				// fold multiplying same vars
				for &factor2, j in a.factors {
					if j == i do continue

					var2 := (&factor2.(Atom_Var)) or_continue
					(v.name == var2.name) or_continue
					
					var2.f = fraction_product(var2.f, v.f)
					unordered_remove(&a.factors, i)
		
					log_debug_update(constrs, "folding vars")
					updated ^= true

					break
				}
			
			case Atom_Add, Atom_Mul, Atom_Div:
				// TODO
			}
		}

		/*
		expand multiplying additions

		(a + b) * (c + d) -> a*c + a*d + b*c + b*d

		TODO: more than 2 factors
		*/
		if len(a.factors) == 2 {
			lhs, is_lhs_add := &a.factors[0].(Atom_Add)
			rhs, is_rhs_add := &a.factors[1].(Atom_Add)
	
			if is_lhs_add && is_rhs_add && len(lhs.addends) == 2 && len(rhs.addends) == 2 {

				addends := make([dynamic]Atom, 4, 12)

				addends[0] = atom_mul_make(
					atom_copy(lhs.addends[0]),
					atom_copy(rhs.addends[0]),
				)

				addends[1] = atom_mul_make(
					atom_copy(lhs.addends[0]),
					atom_copy(rhs.addends[1]),
				)

				addends[2] = atom_mul_make(
					atom_copy(lhs.addends[1]),
					atom_copy(rhs.addends[0]),
				)

				addends[3] = atom_mul_make(
					atom_copy(lhs.addends[1]),
					atom_copy(rhs.addends[1]),
				)
	
				atom ^= Atom_Add{addends}
	
				log_debug_update(constrs, "folding mult of adds")
				updated ^= true
				return // atom is now an add
			}
		}

		// len = 1
		if len(a.factors) == 1 {
			atom ^= a.factors[0]
			log_debug_update(constrs, "single factor")
			updated ^= true
		}
	
	case Atom_Div:

		walk_atom(a.dividend, constr_i, constrs, updated)
		walk_atom(a.divisor,  constr_i, constrs, updated)

		dividend_num, is_dividend_num := a.dividend.(Atom_Num)
		divisor_num,  is_divisor_num  := a.divisor .(Atom_Num)

		// fold dividing zeros
		if is_divisor_num && divisor_num.num == 0 {
			atom ^= Atom_Num{FRACTION_ZERO}
			log_debug_update(constrs, "divide by zero")
			updated ^= true
			return // atom is now a num
		}

		// fold dividing ones
		if is_divisor_num && divisor_num.f == FRACTION_IDENTITY {
			atom ^= a.dividend^
			log_debug_update(constrs, "divide by one")
			updated ^= true
			return // atom is now a num
		}

		// fold dividing nums
		if is_dividend_num && is_divisor_num {
			atom ^= Atom_Num{{dividend_num.num * divisor_num.den, dividend_num.den * divisor_num.num}}
			log_debug_update(constrs, "folding dividing nums")
			updated ^= true
			return // atom is now a num
		}


		dividend_var, is_dividend_var := a.dividend.(Atom_Var)
		divisor_var,  is_divisor_var  := a.divisor .(Atom_Var)

		// fold dividing vars
		if is_dividend_var && is_divisor_var && dividend_var.name == divisor_var.name {
			atom_div(a.dividend, divisor_var.f)
			atom ^= a.dividend^
			log_debug_update(constrs, "folding dividing vars")
			updated ^= true
			return // atom is now a var
		}
	}

	return
}

atom_copy :: proc (src: Atom) -> Atom
{
	switch s in src {
	case Atom_Num: return s
	case Atom_Var: return s
	case Atom_Add:
		a := s
		a.addends = make([dynamic]Atom, len(s.addends), len(s.addends))
		for addend, i in s.addends {
			a.addends[i] = atom_copy(addend)
		}
		return a
	case Atom_Mul:
		a := s
		a.factors = make([dynamic]Atom, len(s.factors), len(s.factors))
		for factor, i in s.factors {
			a.factors[i] = atom_copy(factor)
		}
		return a
	case Atom_Div:
		a := s
		a.dividend = new_atom(atom_copy(s.dividend^))
		a.divisor  = new_atom(atom_copy(s.divisor^))
		return a
	}
	return {}
}

atom_new_copy :: proc (src: Atom) -> ^Atom
{
	return new_atom(atom_copy(src))
}

fraction_sum :: proc (a: Fraction, b: Fraction) -> Fraction
{
	return {a.num * b.den + b.num * a.den, a.den * b.den}
}

fraction_product :: proc (a: Fraction, b: Fraction) -> Fraction
{
	return {a.num * b.num, a.den * b.den}
}

atom_sub_by_atom :: proc (dst: ^Atom, sub: Atom)
{	
	sub := sub
	atom_neg(&sub)
			
	if add, is_add := &dst.(Atom_Add); is_add {
		append(&add.addends, sub)
	} else {
		dst ^= atom_add_make(dst^, sub)
	}
}

atom_div_by_atom :: proc (dst: ^Atom, div: Atom)
{
	if dst_div, dst_is_div := &dst.(Atom_Div); dst_is_div {
		if divisor_mul, divisor_is_mul := &dst_div.divisor.(Atom_Mul); divisor_is_mul {
			append(&divisor_mul.factors, dst_div.divisor^)
		} else {
			dst_div.divisor ^= atom_mul_make(dst_div.divisor^, div)
		}
	} else {
		dst ^= atom_div_make(dst^, div)
	}
}

atom_mul_by_atom :: proc (dst: ^Atom, mul: Atom)
{
	if dst_mul, is_mul := &dst.(Atom_Mul); is_mul {
		append(&dst_mul.factors, mul)
	} else {
		dst ^= atom_mul_make(dst^, mul)
	}
}

atom_mul :: proc (atom: ^Atom, f: Fraction)
{
	assert(f.den != 0, "division by zero")

	switch f {
	case FRACTION_ZERO:
		atom ^= Atom_Num{FRACTION_ZERO}
	case FRACTION_IDENTITY:
		return
	case:
		switch &a in atom {
		case Atom_Num:
			a.num *= f.num
			a.den *= f.den
		case Atom_Var:
			a.f.num *= f.num
			a.f.den *= f.den
		case Atom_Add:
			for &addend in a.addends {
				atom_mul(&addend, f)
			}
		case Atom_Mul:
			assert(len(a.factors) > 0, "empty mul")
			atom_mul(&a.factors[0], f)
		case Atom_Div:
			atom_mul(a.dividend, {f.num, 1})
			atom_mul(a.divisor,  {f.den, 1})
		}
	}
}

atom_div :: proc (atom: ^Atom, f: Fraction)
{
	atom_mul(atom, {f.den, f.num})
}

atom_neg :: proc (atom: ^Atom)
{
	atom_mul(atom, {-1, 1})
}

has_dependencies :: proc (atom: Atom) -> bool
{
	switch a in atom {
	case Atom_Num:
		return false
	case Atom_Var:
		return true
	case Atom_Add:
		for addend in a.addends {
			if has_dependencies(addend) do return true
		}
	case Atom_Mul:
		for factor in a.factors {
			if has_dependencies(factor) do return true
		}
	case Atom_Div:
		return has_dependencies(a.dividend^) || has_dependencies(a.divisor^)
	}
	return false
}

has_dependency :: proc (atom: Atom, var: string) -> bool
{
	switch a in atom {
	case Atom_Num:
		return false
	case Atom_Var:
		return a.name == var
	case Atom_Add:
		for addend in a.addends {
			if has_dependency(addend, var) do return true
		}
	case Atom_Mul:
		for factor in a.factors {
			if has_dependency(factor, var) do return true
		}
	case Atom_Div:
		return has_dependency(a.dividend^, var) || has_dependency(a.divisor^, var)
	}
	return false
}
