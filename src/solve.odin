package bilang

import "core:log"
import "core:math"


Atom :: union #no_nil {
	Atom_Num,
	Atom_Var,
	Atom_Add,
	Atom_Mul,
	Atom_Div,
	Atom_Pow,
}

Fraction :: struct {
	num, den: f64,
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
	top, bot: ^Atom,
}

Atom_Pow :: struct {
	base, exponent: ^Atom,
}

Constraint :: struct {
	var:      string,
	lhs, rhs: ^Atom,
}

FRACTION_IDENTITY: Fraction : {1, 1}
FRACTION_ZERO    : Fraction : {0, 1}


@require_results
new_atom :: proc (atom: Atom) -> ^Atom
{
	a, err := new(Atom)
	if err != nil do log.error("new_atom failed", err)
	a ^= atom
	return a
}
@require_results
atom_num_make :: proc (num: f64, den: f64 = 1) -> Atom_Num
{
	return Atom_Num{{num, den}}
}
@require_results
atom_var_make :: proc (name: string, f: Fraction = FRACTION_IDENTITY) -> Atom_Var
{
	return Atom_Var{name, f}
}
@require_results
atom_add_make :: proc (lhs, rhs: Atom) -> (add: Atom_Add) #no_bounds_check
{
	add.addends = make([dynamic]Atom, 2, 12)
	add.addends[0] = lhs
	add.addends[1] = rhs
	return
}
@require_results
atom_mul_make :: proc (lhs, rhs: Atom) -> (mul: Atom_Mul) #no_bounds_check
{
	mul.factors = make([dynamic]Atom, 2, 12)
	mul.factors[0] = lhs
	mul.factors[1] = rhs
	return
}
@require_results
atom_div_make :: proc (top, bot: Atom) -> (div: Atom_Div)
{
	div.top = new_atom(top)
	div.bot = new_atom(bot)
	return
}
@require_results
atom_pow_make :: proc (base, exponent: Atom) -> (pow: Atom_Pow)
{
	pow.base     = new_atom(base)
	pow.exponent = new_atom(exponent)
	return
}
@require_results
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
		case .Pow:
			return atom_pow_make(lhs, rhs)
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

atom_copy :: proc (src: Atom) -> Atom
{
	switch s in src {
	case Atom_Num, Atom_Var:
		return src
	case Atom_Add:
		a := s
		a.addends = make([dynamic]Atom, len(s.addends))
		for addend, i in s.addends {
			a.addends[i] = atom_copy(addend)
		}
		return a
	case Atom_Mul:
		a := s
		a.factors = make([dynamic]Atom, len(s.factors))
		for factor, i in s.factors {
			a.factors[i] = atom_copy(factor)
		}
		return a
	case Atom_Div:
		a := s
		a.top = new_atom(atom_copy(s.top^))
		a.bot = new_atom(atom_copy(s.bot^))
		return a
	case Atom_Pow:
		a := s
		a.base     = new_atom(atom_copy(s.base^))
		a.exponent = new_atom(atom_copy(s.exponent^))
		return a
	}
	return {}
}

atom_new_copy :: proc (src: Atom) -> ^Atom
{
	return new_atom(atom_copy(src))
}

fraction_sum :: proc (a, b: Fraction) -> Fraction
{
	return {a.num * b.den + b.num * a.den, a.den * b.den}
}

fraction_product :: proc (a, b: Fraction) -> Fraction
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
		if bot_mul, bot_is_mul := &dst_div.bot.(Atom_Mul); bot_is_mul {
			append(&bot_mul.factors, dst_div.bot^)
		} else {
			dst_div.bot ^= atom_mul_make(dst_div.bot^, div)
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
			atom_mul(a.top, {f.num, 1})
			atom_mul(a.bot, {f.den, 1})
		case Atom_Pow:
			// nothing we can do
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

atom_flip :: proc (atom: ^Atom)
{
	switch &a in atom {
	case Atom_Num:
		a.num, a.den = a.den, a.num
	case Atom_Var:
		a.f.num, a.f.den = a.f.den, a.f.num
	case Atom_Div:
		a.top, a.bot = a.bot, a.top
	case Atom_Add, Atom_Mul, Atom_Pow:
		atom^ = atom_div_make(atom^, Atom_Num{{1, 1}})
	}
}

is_dividable_by_var :: proc (a: Atom, var: string) -> bool
{
	switch &v in a {
	case Atom_Num:
		return false
	case Atom_Var:
		return v.name == var
	case Atom_Add:
		for addend in v.addends {
			if !is_dividable_by_var(addend, var) {
				return false
			}
		}
		return true
	case Atom_Mul:
		for factor in v.factors {
			if is_dividable_by_var(factor, var) {
				return true
			}
		}
	case Atom_Div:
		return is_dividable_by_var(v.top^, var) && !has_dependency(v.bot^, var)
	case Atom_Pow:
		return false // TODO
	}
	return false
}

// ! Check is_dividable_by_var before calling !
atom_divide_by_var :: proc (a: ^Atom, var: string)
{
	switch &v in a {
	case Atom_Num:
		panic("unexpected num")
	case Atom_Var:
		a ^= Atom_Num{v.f}
	case Atom_Add:
		for &addend in v.addends {
			atom_divide_by_var(&addend, var)
		}
	case Atom_Mul:
		for &factor in v.factors {
			if is_dividable_by_var(factor, var) {
				atom_divide_by_var(&factor, var)
			}
		}
	case Atom_Div:
		atom_divide_by_var(v.top, var)
	case Atom_Pow:
		// TODO
	}
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
			if has_dependencies(addend) {
				return true
			}
		}
	case Atom_Mul:
		for factor in a.factors {
			if has_dependencies(factor) {
				return true
			}
		}
	case Atom_Div:
		return has_dependencies(a.top^) || has_dependencies(a.bot^)
	case Atom_Pow:
		return has_dependencies(a.base^) || has_dependencies(a.exponent^)
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
			if has_dependency(addend, var) {
				return true
			}
		}
	case Atom_Mul:
		for factor in a.factors {
			if has_dependency(factor, var) {
				return true
			}
		}
	case Atom_Div:
		return has_dependency(a.top^, var) || has_dependency(a.bot^, var)
	case Atom_Pow:
		return has_dependency(a.base^, var) || has_dependency(a.exponent^, var)
	}
	return false
}

// Compares structurally
contraint_equals :: proc (a, b: Constraint) -> bool
{
	return a.var == b.var && atom_equals(a.lhs^, b.lhs^) && atom_equals(a.rhs^, b.rhs^)
}

// Compares structurally
atom_equals :: proc (a_atom, b_atom: Atom) -> bool
{
	switch a in a_atom {
	case Atom_Num:
		b, is_b_num := b_atom.(Atom_Num)
		return is_b_num && a == b
	case Atom_Var:
		b, is_b_var := b_atom.(Atom_Var)
		return is_b_var && a == b
	case Atom_Add:
		b, is_b_add := b_atom.(Atom_Add)
		return is_b_add || slice_equals_by(a.addends[:], b.addends[:], atom_equals)
	case Atom_Mul:
		b, is_b_mul := b_atom.(Atom_Mul)
		return is_b_mul || slice_equals_by(a.factors[:], b.factors[:], atom_equals)
	case Atom_Div:
		b, is_b_div := b_atom.(Atom_Div)
		return is_b_div && atom_equals(a.top^, b.top^) && atom_equals(a.bot^, b.bot^)
	case Atom_Pow:
		b, is_b_pow := b_atom.(Atom_Pow)
		return is_b_pow && atom_equals(a.base^, b.base^) && atom_equals(a.exponent^, b.exponent^)
	case:
		return false
	}
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
			constr.lhs = new_atom(atom_copy(lhs))
			constr.rhs = new_atom(atom_copy(rhs))
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
		// move addends to rhs if they don't depend on var
		#reverse for &a, i in lhs.addends {
			if has_dependency(a, constr.var) do continue

			atom_sub_by_atom(constr.rhs, a)
			unordered_remove(&lhs.addends, i)

			log_debug_update(constrs, "subtracting addend lhs")
			updated ^= true
		}

		/*
		try extracting var from addends
		2a + 3ab = 1  ->  a(2 + 3b) = 1  ->  a = 1 / (2 + 3b)
		*/
		extract: {
			// all addends need to be dividable by var
			for a in lhs.addends {
				if !is_dividable_by_var(a, constr.var) {
					break extract
				}
			}

			// remove var from addends
			for &a in lhs.addends {
				atom_divide_by_var(&a, constr.var)
			}

			// move addends to rhs
			atom_div_by_atom(constr.rhs, lhs)

			// lhs is now var
			constr.lhs ^= Atom_Var{constr.var, FRACTION_IDENTITY}

			log_debug_update(constrs, "extracting var from addends")
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
		atom_mul_by_atom(constr.rhs, lhs.bot^)
		constr.lhs = lhs.top
		log_debug_update(constrs, "moved lhs div to rhs")
		updated ^= true

	case Atom_Pow:
		/*
		move exponent to the right
		x^2 = y  ->  x = y^(1/2)
		*/
		if !has_dependency(lhs.exponent^, constr.var) {
			atom_flip(lhs.exponent)
			constr.lhs, constr.rhs, lhs.base = lhs.base, constr.lhs, constr.rhs
		}
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

			log_debug_update(constrs, "subtracting addend rhs")
			updated ^= true
		}

	case Atom_Mul:
		// ignore

	case Atom_Div:
		// ignore

	case Atom_Pow:
		// ignore
	}
}

walk_atom :: proc (atom: ^Atom, constr_i: int, constrs: []Constraint, updated: ^bool)
{
	switch &a in atom {
	case Atom_Num:
		// a.num /= a.den

	case Atom_Var:
		/* leave substitution as last step */
		if updated^ do break

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
				/* fold adding zeros */
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
					    (a / b) + 3/2  ->  (2a + 3b) / 2b
						*/
						rhs := atom_copy(v2.bot^)
						atom_mul(v2.top, {v.f.den, 1})
						atom_mul(&rhs,   {v.f.num, 1})
						atom_mul(v2.bot, {v.f.den, 1})
						v2.top ^= atom_add_make(v2.top^, rhs)
					case Atom_Var, Atom_Mul, Atom_Pow:
						continue
					}

					unordered_remove(&a.addends, i)

					log_debug_update(constrs, "folding adding num")
					updated ^= true
					break
				}

			case Atom_Var:
				/* fold adding same vars */
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
				/* fold adding additions */
				for &addend2, j in a.addends {
					if j == i do continue

					add2 := (&addend2.(Atom_Add)) or_continue

					append(&add2.addends, ..v.addends[:])
					unordered_remove(&a.addends, i)

					log_debug_update(constrs, "folding adding additions")
					updated ^= true
				}

			case Atom_Mul, Atom_Div, Atom_Pow:
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
				
				/* fold multiplying zeros */
				if v.num == 0 {
					atom ^= Atom_Num{FRACTION_ZERO}
					log_debug_update(constrs, "multiply by zero")
					updated ^= true
					return // atom is now a num
				}

				/* fold multiplying ones */
				if v.f == FRACTION_IDENTITY && len(a.factors) > 1 {
					unordered_remove(&a.factors, i)
	
					log_debug_update(constrs, "removing one")
					updated ^= true
					break
				}

				/* fold multiplying num */
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
					case Atom_Mul, Atom_Div, Atom_Pow:
						continue
					}

					unordered_remove(&a.factors, i)
					log_debug_update(constrs, "fold multiplying num")
					updated ^= true
					break
				}
			
			case Atom_Var:
				/* fold multiplying the same vars */
				for &factor2, j in a.factors {
					v2, is_v2_var := &factor2.(Atom_Var)
					if j == i || !is_v2_var || v.name != v2.name do continue

					base     := atom_var_make(v.name)
					exponent := atom_num_make(2)
					pow      := atom_pow_make(base, exponent)

					// x * x  ->  x^2
					if v.f == v2.f && v.f == FRACTION_IDENTITY {
						a.factors[j] = pow
					}\
					// 2x * 3x  ->  6 * x^2
					else {
						num := Atom_Num{fraction_product(v.f, v2.f)}
						mul := atom_mul_make(num, pow)
						a.factors[j] = mul
					}

					unordered_remove(&a.factors, i)
					log_debug_update(constrs, "fold multiplying same vars")
					updated ^= true
					break
				}
			
			case Atom_Add, Atom_Mul, Atom_Div, Atom_Pow:
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

		walk_atom(a.top, constr_i, constrs, updated)
		walk_atom(a.bot, constr_i, constrs, updated)

		switch bot in a.bot {
		case Atom_Num:

			switch top in a.top {
			case Atom_Num: atom_div(a.top, bot.f)
			case Atom_Var: atom_div(a.top, bot.f)
			case Atom_Div: atom_mul(top.bot, bot.f)
			case Atom_Mul: atom_div(a.top, bot.f)
			case Atom_Add: atom_div(a.top, bot.f)
			case Atom_Pow:
				return // nothing we can do
			}

			atom ^= a.top^
			log_debug_update(constrs, "folding dividing by num")
			updated ^= true
			return // atom is top atom
			
		case Atom_Var:
			top_var, is_top_var := a.top.(Atom_Var)
			
			if is_top_var && top_var.name == bot.name {
				atom_div(a.top, bot.f)
				atom ^= a.top^
				log_debug_update(constrs, "folding dividing by var")
				updated ^= true
				return // atom is now a var
			}
		case Atom_Mul, Atom_Div, Atom_Add, Atom_Pow:
			// TODO
		}

		// 0/x -> 0
		if top_num, is_top_num := a.top.(Atom_Num); is_top_num && top_num.f.num == 0 {
			atom ^= Atom_Num{FRACTION_ZERO}
			log_debug_update(constrs, "dividing zero")
			updated ^= true
		}
	case Atom_Pow:
		walk_atom(a.base    , constr_i, constrs, updated)
		walk_atom(a.exponent, constr_i, constrs, updated)

		// fold num pow
		// 2^3 -> 8
		base_num    , is_base_num     := a.base.(Atom_Num)
		exponent_num, is_exponent_num := a.exponent.(Atom_Num)

		if is_base_num && is_exponent_num {
			num := base_num
			num.num = math.pow(num.num, exponent_num.num/exponent_num.den)
			atom ^= num

			log_debug_update(constrs, "fold num pow")
			updated ^= true
		}
	}

	return
}
