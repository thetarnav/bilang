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
	Atom_Binary,
}

Fraction :: struct {
	num: f64,
	den: f64,
}

Atom_Num :: struct {
	using f: Fraction,
}

Atom_Var :: struct {
	using f: Fraction,
	name: string,
}

Atom_Binary :: struct {
	lhs:  ^Atom,
	op:   Atom_Binary_Op,
	rhs:  ^Atom,
}

Atom_Binary_Op :: enum {
	Add,
	Mul,
	Div,
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
	a := new(Atom)
	a ^= atom
	return a
}

atom_from_expr :: proc (
	expr:             Expr,
	all_constraints:  ^[dynamic]Constraint,
	decl_start:       int,                  // index into all_constraints
) -> Atom
{
	switch v in expr {
	case ^Expr_Ident:
		var := Atom_Var{
			name = v.name,
			f    = FRACTION_IDENTITY,
		}
		// var already in decl constraints
		for c in all_constraints[decl_start:] {
			if c.var == v.name do return var
		}
		// var not in decl constraints
		append(all_constraints, Constraint{var = v.name})

		return var

	case ^Expr_Number:
		return Atom_Num{
			num = v.value,
			den = 1,
		}
	case ^Expr_Binary:
		bin := Atom_Binary{
			lhs = new_atom(atom_from_expr(v.lhs, all_constraints, decl_start)),
			rhs = new_atom(atom_from_expr(v.rhs, all_constraints, decl_start)),
		}
		
		switch v.op {
		case .Add:
			bin.op = .Add
		case .Sub:
			bin.op = .Add
			atom_neg(bin.rhs)
		case .Mul:
			bin.op = .Mul
		case .Div:
			bin.op = .Div
		}

		return bin
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

	return Atom_Num{}
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

		// b := strings.builder_make_len_cap(0, 1024)
		// w := strings.to_writer(&b)
	
		// write_contraints(w, constrs[:])
	
		// output := strings.to_string(b)

		// fmt.println(output)

		if !updated do break
	}

	return constrs[:]
}

walk_constraint :: proc (constr_i: int, constrs: []Constraint, updated: ^bool)
{
	constr := &constrs[constr_i]
	
	walk_atom(constr.lhs, constr_i, constrs, updated)
	walk_atom(constr.rhs, constr_i, constrs, updated)

	lhs_has_var := has_dependency(constr.lhs^, constr.var)
	rhs_has_var := has_dependency(constr.rhs^, constr.var)


	/*
	Move top-level additions between lhs and rhs
	so that the var is on the left side,
	and everything else is on the right side
	*/
	#partial switch &lhs in constr.lhs {
	case Atom_Binary:
		#partial switch &rhs in constr.rhs {
		case Atom_Binary:
			if lhs.op == .Add && rhs.op == .Add {

				move_add :: proc (
					from    : ^Atom_Binary,
					to      : ^Atom_Binary,
					var_name: string,
					side    : enum {LHS, RHS},
					updated : ^bool,
				) {
					assert(from.op == .Add)

					new_bin := Atom_Binary{op=.Add}

					lhs_block: {
						switch &lhs in from.lhs {
						case Atom_Num:
							if side == .RHS do break lhs_block

						case Atom_Var:
							if (side == .RHS && lhs.name != var_name) ||
							   (side == .LHS && lhs.name == var_name) {
								break lhs_block
							}

						case Atom_Binary:
							if lhs.op == .Add {
								move_add(&lhs, to, var_name, side, updated)
								break lhs_block
							} else
							if (side == .RHS && !has_dependency(lhs, var_name)) ||
							   (side == .LHS &&  has_dependency(lhs, var_name)) {
								break lhs_block
							}
						}

						new_bin.lhs = to.rhs   // reusing pointers
						new_bin.rhs = from.lhs // reusing pointers
						atom_neg(new_bin.rhs)
						to.rhs   = new_atom(new_bin)
						from.lhs = new_atom(Atom_Num{FRACTION_ZERO})
						updated  ^= true

						return
					}

					rhs_block: {
						switch &rhs in from.rhs {
						case Atom_Num:
							if side == .RHS do break rhs_block

						case Atom_Var:
							if side == .RHS && rhs.name != var_name ||
							   side == .LHS && rhs.name == var_name {
								break rhs_block
							}

						case Atom_Binary:
							if rhs.op == .Add {
								move_add(&rhs, to, var_name, side, updated)
								break rhs_block
							} else
							if (side == .RHS && !has_dependency(rhs, var_name)) ||
							   (side == .LHS &&  has_dependency(rhs, var_name)) {
								break rhs_block
							}
						}

						new_bin.lhs = to.rhs   // reusing pointers
						new_bin.rhs = from.rhs // reusing pointers
						atom_neg(new_bin.rhs)
						to.rhs   = new_atom(new_bin)
						from.rhs = new_atom(Atom_Num{FRACTION_ZERO})
						updated ^= true

						return
					}
				}
				
				move_add(&lhs, &rhs, constr.var, .LHS, updated)
				move_add(&rhs, &lhs, constr.var, .RHS, updated)

				if updated^ {
					log_debug_update(constrs, "moving adds")
					return
				}
			}	
		}
	}

	if lhs_has_var && rhs_has_var {
		return
	}


	if rhs_has_var {
		constr.lhs, constr.rhs = constr.rhs, constr.lhs
	}

	switch_sides: switch &lhs in constr.lhs {
	case Atom_Num:
		// ignore
	case Atom_Var:
		if lhs.f == FRACTION_IDENTITY do break
		
		atom_div(constr.rhs, lhs.f)
		lhs.f = FRACTION_IDENTITY
	
		log_debug_update(constrs, "dividing rhs by lhs var")
		updated ^= true
	case Atom_Binary:

		new_bin: Atom_Binary

		switch lhs.op {
		case .Add:
			new_bin.op = .Add
			switch {
			case !has_dependencies(lhs.lhs^):
				new_bin.rhs = lhs.lhs
				constr.lhs  = lhs.rhs
			case !has_dependencies(lhs.rhs^):
				new_bin.rhs = lhs.rhs
				constr.lhs  = lhs.lhs
			case:
				break switch_sides
			}
			atom_neg(new_bin.rhs)
		case .Mul:
			new_bin.op = .Div
			switch {
			case !has_dependencies(lhs.lhs^):
				new_bin.rhs = lhs.lhs
				constr.lhs  = lhs.rhs
			case !has_dependencies(lhs.rhs^):
				new_bin.rhs = lhs.rhs
				constr.lhs  = lhs.lhs
			case:
				break switch_sides
			}
		case .Div:
			new_bin.op  = .Mul
			new_bin.rhs = lhs.rhs
			constr.lhs  = lhs.lhs
		}

		new_bin.lhs = constr.rhs
		constr.rhs  = new_atom(new_bin)
	
		log_debug_update(constrs, "switching sides")
		updated ^= true
	}
}

walk_atom :: proc (atom: ^Atom, constr_i: int, constrs: []Constraint, updated: ^bool)
{
	switch &a in atom {
	case Atom_Num:
		// a.num /= a.den
	case Atom_Var:
		constr := &constrs[constr_i]

		for c, i in constrs {
			if i == constr_i || a.name == constr.var do continue

			var: Atom_Var
			copy: Atom
			if lhs_var, is_lhs_var := c.lhs.(Atom_Var); is_lhs_var && lhs_var.name == a.name && c.rhs != constr.rhs && !has_dependencies(c.rhs^) {
				var  = lhs_var
				copy = atom_copy(c.rhs^)
			} else
			if rhs_var, is_rhs_var := c.rhs.(Atom_Var); is_rhs_var && rhs_var.name == a.name && c.lhs != constr.lhs && !has_dependencies(c.lhs^) {
				var  = rhs_var
				copy = atom_copy(c.lhs^)
			} else {
				continue
			}

			atom_div(&copy, var.f)
			atom_mul(&copy, a.f)
			atom ^= copy

			log_debug_update(constrs, "substituting var")
			updated ^= true
		}
	case Atom_Binary:
		walk_atom(a.lhs, constr_i, constrs, updated)
		walk_atom(a.rhs, constr_i, constrs, updated)

		lhs_num, is_lhs_num := a.lhs.(Atom_Num)
		rhs_num, is_rhs_num := a.rhs.(Atom_Num)

		if is_lhs_num && is_rhs_num {
			switch a.op {
			case .Add: atom ^= Atom_Num{{lhs_num.num * rhs_num.den + rhs_num.num * lhs_num.den, lhs_num.den * rhs_num.den}}
			case .Mul: atom ^= Atom_Num{{lhs_num.num * rhs_num.num, lhs_num.den * rhs_num.den}}
			case .Div: atom ^= Atom_Num{{lhs_num.num * rhs_num.den, lhs_num.den * rhs_num.num}}
			}

			log_debug_update(constrs, "folding nums")
			updated ^= true
			return
		}

		if is_rhs_num {
			switch a.op {
			case .Add:
				if rhs_num.num == 0 {
					atom ^= a.lhs^
					log_debug_update(constrs, "adding zero")
					updated ^= true
					return
				}
			case .Div:
				atom_div(a.lhs, rhs_num)
				atom ^= a.lhs^

				log_debug_update(constrs, "fold div")
				updated ^= true
				return
			case .Mul:
				atom_mul(a.lhs, rhs_num)
				atom ^= a.lhs^

				log_debug_update(constrs, "fold mul")
				updated ^= true
				return
			}
		}

		if is_lhs_num {
			switch a.op {
			case .Add:
				if lhs_num.num == 0 {
					atom ^= a.rhs^
					log_debug_update(constrs, "adding zero")
					updated ^= true
					return
				}
			case .Div:
				// cannot be folded here
			case .Mul:
				atom_mul(a.rhs, lhs_num)
				atom ^= a.rhs^

				log_debug_update(constrs, "fold mul")
				updated ^= true
				return
			}
		}


		/*
		fold adding same vars
		*/
		lhs_var, is_lhs_var := a.lhs.(Atom_Var)
		rhs_var, is_rhs_var := a.rhs.(Atom_Var)

		if is_lhs_var && is_rhs_var && lhs_var.name == rhs_var.name {
			switch a.op {
			case .Add:
				atom ^= Atom_Var{
					name = lhs_var.name,
					f    = {lhs_var.num * rhs_var.den + rhs_var.num * lhs_var.den, lhs_var.den * rhs_var.den},
				}

				log_debug_update(constrs, "folding vars")
				updated ^= true	
				return
			case .Mul, .Div:
				// TODO
				// might require squaring
			}
		}

		/*
		fold multiplying additions

		(a + b) * (c + d) -> a*c + a*d + b*c + b*d
		*/

		lhs_bin, is_lhs_bin := &a.lhs.(Atom_Binary)
		rhs_bin, is_rhs_bin := &a.rhs.(Atom_Binary)

		if a.op == .Mul && is_lhs_bin && is_rhs_bin && lhs_bin.op == .Add && rhs_bin.op == .Add
		{
			ac := new_atom(Atom_Binary{
				lhs = atom_new_copy(lhs_bin.lhs^),
				op  = .Mul,
				rhs = atom_new_copy(rhs_bin.lhs^),
			})

			ad := new_atom(Atom_Binary{
				lhs = atom_new_copy(lhs_bin.lhs^),
				op  = .Mul,
				rhs = atom_new_copy(rhs_bin.rhs^),
			})

			bc := new_atom(Atom_Binary{
				lhs = atom_new_copy(lhs_bin.rhs^),
				op  = .Mul,
				rhs = atom_new_copy(rhs_bin.lhs^),
			})

			bd := new_atom(Atom_Binary{
				lhs = atom_new_copy(lhs_bin.rhs^),
				op  = .Mul,
				rhs = atom_new_copy(rhs_bin.rhs^),
			})

			add1 := new_atom(Atom_Binary{
				lhs = ac,
				op  = .Add,
				rhs = ad,
			})

			add2 := new_atom(Atom_Binary{
				lhs = bc,
				op  = .Add,
				rhs = bd,
			})

			atom ^= Atom_Binary{
				lhs = add1,
				op  = .Add,
				rhs = add2,
			}

			log_debug_update(constrs, "folding mult of adds")
			updated ^= true
			return
		}
	}

	return
}

atom_copy :: proc (src: Atom) -> Atom
{
	switch s in src {
	case Atom_Num: return s
	case Atom_Var: return s
	case Atom_Binary:
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
			a.num *= f.num
			a.den *= f.den
		case Atom_Binary:
			switch a.op {
			case .Add:
				atom_mul(a.lhs, f)
				atom_mul(a.rhs, f)
			case .Mul:
				atom_mul(a.lhs, f)
			case .Div:
				atom_mul(a.lhs, {f.num, 1})
				atom_mul(a.rhs, {f.den, 1})
			}
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
	case Atom_Binary:
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
	case Atom_Binary:
		return has_dependency(a.lhs^, var) || has_dependency(a.rhs^, var)
	}
	return false
}
