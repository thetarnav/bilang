package bilang

import "base:runtime"
import "core:log"
import "core:math"
import "core:strings"


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
	lhs, rhs: ^Atom,
}

Atom_Pow :: struct {
	lhs, rhs: ^Atom,
}

Constraint :: struct {
	var:      string,
	lhs, rhs: ^Atom,
}

FRACTION_IDENTITY :: Fraction{1, 1}
FRACTION_ZERO     :: Fraction{0, 1}


@require_results
new_atom :: proc (atom: Atom) -> ^Atom
{
	a, err := new(Atom)
	if err != nil do log.error("new_atom failed", err)
	a^ = atom
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
atom_div_make :: proc (lhs, rhs: Atom) -> (div: Atom_Div)
{
	div.lhs = new_atom(lhs)
	div.rhs = new_atom(rhs)
	return
}
@require_results
atom_pow_make :: proc (lhs, rhs: Atom) -> (pow: Atom_Pow)
{
	pow.lhs = new_atom(lhs)
	pow.rhs = new_atom(rhs)
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
		a.lhs = new_atom(atom_copy(s.lhs^))
		a.rhs = new_atom(atom_copy(s.rhs^))
		return a
	case Atom_Pow:
		a := s
		a.lhs = new_atom(atom_copy(s.lhs^))
		a.rhs = new_atom(atom_copy(s.rhs^))
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
	sub := atom_copy(sub)
	atom_neg(&sub)
			
	if add, is_add := &dst.(Atom_Add); is_add {
		append(&add.addends, sub)
	} else {
		dst^ = atom_add_make(dst^, sub)
	}
}

atom_div_by_atom :: proc (dst: ^Atom, div: Atom)
{
	if dst_div, dst_is_div := &dst.(Atom_Div); dst_is_div {
		if rhs_mul, rhs_is_mul := &dst_div.rhs.(Atom_Mul); rhs_is_mul {
			append(&rhs_mul.factors, dst_div.rhs^)
		} else {
			dst_div.rhs^ = atom_mul_make(dst_div.rhs^, div)
		}
	} else {
		dst^ = atom_div_make(dst^, div)
	}
}

atom_mul_by_atom :: proc (dst: ^Atom, mul: Atom)
{
	if dst_mul, is_mul := &dst.(Atom_Mul); is_mul {
		append(&dst_mul.factors, mul)
	} else {
		dst^ = atom_mul_make(dst^, mul)
	}
}

atom_mul :: proc (atom: ^Atom, f: Fraction)
{
	assert(f.den != 0, "division by zero")

	switch f {
	case FRACTION_ZERO:
		atom^ = Atom_Num{FRACTION_ZERO}
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
			atom_mul(a.lhs, {f.num, 1})
			atom_mul(a.rhs, {f.den, 1})
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
		a.rhs, a.lhs = a.lhs, a.rhs
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
		return is_dividable_by_var(v.lhs^, var) && !has_dependency(v.rhs^, var)
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
		a^ = Atom_Num{v.f}
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
		atom_divide_by_var(v.lhs, var)
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
		return has_dependencies(a.lhs^) || has_dependencies(a.rhs^)
	case Atom_Pow:
		return has_dependencies(a.lhs^) || has_dependencies(a.rhs^)
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
		return has_dependency(a.lhs^, var) || has_dependency(a.rhs^, var)
	case Atom_Pow:
		return has_dependency(a.lhs^, var) || has_dependency(a.rhs^, var)
	}
	return false
}

has_dependency_other_than_var :: proc (atom: Atom, var: string) -> bool {
	switch a in atom {
	case Atom_Num:
		return false
	case Atom_Var:
		return a.name != var
	case Atom_Add:
		for addend in a.addends {
			if has_dependency_other_than_var(addend, var) {
				return true
			}
		}
	case Atom_Mul:
		for factor in a.factors {
			if has_dependency_other_than_var(factor, var) {
				return true
			}
		}
	case Atom_Div:
		return has_dependency_other_than_var(a.lhs^, var) || has_dependency_other_than_var(a.rhs^, var)
	case Atom_Pow:
		return has_dependency_other_than_var(a.lhs^, var) || has_dependency_other_than_var(a.rhs^, var)
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
		return is_b_div && atom_equals(a.lhs^, b.lhs^) && atom_equals(a.rhs^, b.rhs^)
	case Atom_Pow:
		b, is_b_pow := b_atom.(Atom_Pow)
		return is_b_pow && atom_equals(a.lhs^, b.lhs^) && atom_equals(a.rhs^, b.rhs^)
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

	/*
	Display additional information about current state of constraints when debugging
	*/
	when ODIN_DEBUG {
		Logger_Data :: struct {
			logger_proc: runtime.Logger_Proc,
			logger_data: rawptr,
			constrs:     ^[dynamic]Constraint,
		}
		logger_data := Logger_Data{
			logger_proc = context.logger.procedure,
			logger_data = context.logger.data,
			constrs     = &constrs,
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

	solve_loop: for {
		updated: bool

		for _, i in constrs {
			walk_constraint(i, constrs[:], &updated)
		}

		if updated do continue

		// try substituting solved vars
		for constr, constr_i in constrs {

			lhs_var, is_lhs_var := constr.lhs.(Atom_Var)
			if !is_lhs_var || has_dependencies(constr.rhs^) {
				continue
			}

			for constr2, constr2_i in constrs {
				if constr_i   != constr2_i &&
				   constr.var != constr2.var
				{
					try_substituting_var(constr2.lhs, lhs_var, constr.rhs^, &updated)
					try_substituting_var(constr2.rhs, lhs_var, constr.rhs^, &updated)
				}
			}
		}

		if updated do continue

		// // try approximation
		// for &constr in constrs {
		// 	if try_finding_approximate_solution(&constr) {
		// 		continue solve_loop
		// 	}
		// }
		
		break
	}

	return constrs[:]
}

try_substituting_var :: proc (atom: ^Atom, var: Atom_Var, value: Atom, updated: ^bool)
{
	switch &a in atom {
	case Atom_Num:
		// skip
	case Atom_Var:
		if a.name == var.name {
			value := atom_copy(value)
			atom_div(&value, var.f)
			atom_mul(&value, a.f)
			atom^ = value

			log_debug("substituting var")
			updated^ = true
		}
	case Atom_Add:
		for &addend in a.addends {
			try_substituting_var(&addend, var, value, updated)
		}
	case Atom_Mul:
		for &factor in a.factors {
			try_substituting_var(&factor, var, value, updated)
		}
	case Atom_Div:
		try_substituting_var(a.lhs, var, value, updated)
		try_substituting_var(a.rhs, var, value, updated)
	case Atom_Pow:
		try_substituting_var(a.lhs, var, value, updated)
		try_substituting_var(a.rhs, var, value, updated)
	}
}

walk_constraint :: proc (constr_i: int, constrs: []Constraint, updated: ^bool)
{
	#no_bounds_check constr := &constrs[constr_i]
	
	walk_atom(constr.lhs, updated)
	walk_atom(constr.rhs, updated)


	/*
	Keep the var on the left side
	*/
	if !has_dependency(constr.lhs^, constr.var) && has_dependency(constr.rhs^, constr.var)
	{
		constr.lhs, constr.rhs = constr.rhs, constr.lhs

		log_debug("swapping sides")
		updated^ = true
	}


	switch &lhs in constr.lhs {
	case Atom_Num:
		// ignore

	case Atom_Var:
		if lhs.f == FRACTION_IDENTITY do break
		
		atom_div(constr.rhs, lhs.f)
		lhs.f = FRACTION_IDENTITY
	
		log_debug("dividing rhs by lhs var")
		updated^ = true
		
	case Atom_Add:
		// move addends to rhs if they don't depend on var
		#reverse for &a, i in lhs.addends {
			if has_dependency(a, constr.var) do continue

			atom_sub_by_atom(constr.rhs, a)
			unordered_remove(&lhs.addends, i)

			log_debug("subtracting addend lhs")
			updated^ = true
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
			constr.lhs^ = Atom_Var{constr.var, FRACTION_IDENTITY}

			log_debug("extracting var from addends")
			updated^ = true
		}


	case Atom_Mul:
		#reverse for &a, i in lhs.factors {
			if has_dependency(a, constr.var) do continue

			atom_div_by_atom(constr.rhs, a)
			unordered_remove(&lhs.factors, i)

			log_debug("dividing by factor lhs")
			updated^ = true
		}

	case Atom_Div:
		atom_mul_by_atom(constr.rhs, lhs.rhs^)
		constr.lhs = lhs.lhs
		log_debug("moved lhs div to rhs")
		updated^ = true

	case Atom_Pow:
		/*
		move exponent to the right
		x^2 = y  ->  x = y^(1/2)
		*/
		if !has_dependency(lhs.rhs^, constr.var) {
			atom_flip(lhs.rhs)
			constr.lhs, constr.rhs, lhs.lhs = lhs.lhs, constr.lhs, constr.rhs

			log_debug("moved exponent to rhs")
			updated^ = true
		}
	}

	if updated^ {
		walk_atom(constr.lhs, updated)
		walk_atom(constr.rhs, updated)
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

			log_debug("subtracting addend rhs")
			updated^ = true
		}

	case Atom_Mul:
		// ignore

	case Atom_Div:
		// ignore

	case Atom_Pow:
		// ignore
	}
}

walk_atom :: proc (atom: ^Atom, updated: ^bool)
{
	switch &a in atom {
	case Atom_Num:
		// a.num /= a.den

	case Atom_Var:
		// skip

	case Atom_Add:
		#reverse for &addend, i in a.addends {

			walk_atom(&addend, updated)

			switch &v in addend {
			case Atom_Num:
				/* fold adding zeros */
				if v.num == 0 {
					unordered_remove(&a.addends, i)
					log_debug("removing zero")
					updated^ = true
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
						rhs := atom_copy(v2.rhs^)
						atom_mul(v2.lhs, {v.f.den, 1})
						atom_mul(&rhs,   {v.f.num, 1})
						atom_mul(v2.rhs, {v.f.den, 1})
						v2.lhs^ = atom_add_make(v2.lhs^, rhs)
					case Atom_Var, Atom_Mul, Atom_Pow:
						continue
					}

					unordered_remove(&a.addends, i)

					log_debug("folding adding num")
					updated^ = true
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

					log_debug("folding adding same vars")
					updated^ = true
				}

			case Atom_Add:
				/*
				fold adding additions
				a + (b + c)  ->  a + b + c
				*/
				to_add := v.addends[:]
				unordered_remove(&a.addends, i)
				append(&a.addends, ..to_add)

				log_debug("folding adding additions")
				updated^ = true

			case Atom_Mul, Atom_Div, Atom_Pow:
				// TODO
			}
		}

		// len = 1
		if len(a.addends) == 1 {
			atom^ = a.addends[0]
			log_debug("single addend")
			updated^ = true	
		}

	case Atom_Mul:
		walk_atom_mul(atom, &a, updated)
	
	case Atom_Div:

		walk_atom(a.lhs, updated)
		walk_atom(a.rhs, updated)

		switch bot in a.rhs {
		case Atom_Num:

			switch top in a.lhs {
			case Atom_Num: atom_div(a.lhs, bot.f)
			case Atom_Var: atom_div(a.lhs, bot.f)
			case Atom_Div: atom_mul(top.rhs, bot.f)
			case Atom_Mul: atom_div(a.lhs, bot.f)
			case Atom_Add: atom_div(a.lhs, bot.f)
			case Atom_Pow:
				return // nothing we can do
			}

			atom^ = a.lhs^
			log_debug("folding dividing by num")
			updated^ = true
			return // atom is top atom
			
		case Atom_Var:
			top_var, is_top_var := a.lhs.(Atom_Var)
			
			if is_top_var && top_var.name == bot.name {
				atom_div(a.lhs, bot.f)
				atom^ = a.lhs^
				log_debug("folding dividing by var")
				updated^ = true
				return // atom is now a var
			}
		case Atom_Mul, Atom_Div, Atom_Add, Atom_Pow:
			// TODO
		}

		// 0/x -> 0
		if top_num, is_top_num := a.lhs.(Atom_Num); is_top_num && top_num.f.num == 0 {
			atom^ = Atom_Num{FRACTION_ZERO}
			log_debug("dividing zero")
			updated^ = true
		}
	case Atom_Pow:
		walk_atom(a.lhs, updated)
		walk_atom(a.rhs, updated)

		// fold num pow
		// 2^3 -> 8
		base_num    , is_base_num     := a.lhs.(Atom_Num)
		exponent_num, is_exponent_num := a.rhs.(Atom_Num)

		if is_base_num && is_exponent_num {
			num := base_num
			num.num = math.pow(num.num, exponent_num.num/exponent_num.den)
			atom^ = num

			log_debug("fold num pow")
			updated^ = true
		}
	}

	return
}

walk_atom_mul :: proc (atom: ^Atom, mul: ^Atom_Mul, updated: ^bool)
{
	#reverse for &factor, i in mul.factors {

		walk_atom(&factor, updated)

		switch v in factor {
		case Atom_Num:
			
			/* fold multiplying zeros */
			if v.num == 0 {
				atom^ = Atom_Num{FRACTION_ZERO}
				log_debug("multiply by zero")
				updated^ = true
				return // atom is now a num
			}

			/* fold multiplying ones */
			if v.f == FRACTION_IDENTITY && len(mul.factors) > 1 {
				unordered_remove(&mul.factors, i)

				log_debug("removing one")
				updated^ = true
				break
			}

			/* fold multiplying num */
			for &factor2, j in mul.factors {
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

				unordered_remove(&mul.factors, i)
				log_debug("fold multiplying num")
				updated^ = true
				break
			}
		
		case Atom_Var:
			/* fold multiplying the same vars */
			for &factor2, j in mul.factors {
				v2, is_v2_var := &factor2.(Atom_Var)
				if j == i || !is_v2_var || v.name != v2.name do continue

				base     := atom_var_make(v.name)
				exponent := atom_num_make(2)
				pow      := atom_pow_make(base, exponent)

				// x * x  ->  x^2
				if v.f == v2.f && v.f == FRACTION_IDENTITY {
					mul.factors[j] = pow
				}\
				// 2x * 3x  ->  6 * x^2
				else {
					num := Atom_Num{fraction_product(v.f, v2.f)}
					mul.factors[j] = atom_mul_make(num, pow)
				}

				unordered_remove(&mul.factors, i)
				log_debug("fold multiplying same vars")
				updated^ = true
				break
			}

		case Atom_Pow:
			/*
			increase power
			x^2 * x  ->  x^3
			*/
			for &factor2, j in mul.factors {
				if j == i || !atom_equals(v.lhs^, factor2) do continue

				v.rhs^ = atom_add_make(v.rhs^, Atom_Num{{1, 1}})
				mul.factors[j] = v

				unordered_remove(&mul.factors, i)
				log_debug("increase power")
				updated^ = true
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
	if len(mul.factors) == 2 {
		lhs, is_lhs_add := &mul.factors[0].(Atom_Add)
		rhs, is_rhs_add := &mul.factors[1].(Atom_Add)

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

			atom^ = Atom_Add{addends}

			log_debug("folding mult of adds")
			updated^ = true
			return // atom is now an add
		}
	}

	// len = 1
	if len(mul.factors) == 1 {
		atom^ = mul.factors[0]
		log_debug("single factor")
		updated^ = true
	}
}

// try_finding_approximate_solution :: proc (constr: ^Constraint) -> bool
// {
// 	if has_dependency_other_than_var(constr.lhs^, constr.var) ||
// 	   has_dependency_other_than_var(constr.rhs^, constr.var) {
// 		return false
// 	}

// 	/*
// 	move right to the left to have a single atom equals 0
// 	x^2 + x = 12  ->  x^2 + x + -12 = 0
// 	*/
// 	atom := atom_copy(constr.lhs^) // rhs is copied by atom_sub_by_atom
// 	atom_sub_by_atom(&atom, constr.rhs^)

// 	// fold whats possible
// 	for {
// 		updated: bool
// 		walk_atom(&atom, &updated)
// 		updated or_break
// 	}

// 	Float :: bit_field u32 {
// 		sign: i8  |  1,
// 		bits: u32 | 31,
// 	}


// 	return false
// }
