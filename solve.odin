package bilang


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
	using f: Fraction,
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
			f   = FRACTION_IDENTITY,
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
		if !updated do break
	}

	return constrs[:]
}

walk_constraint :: proc (constr_i: int, constrs: []Constraint, updated: ^bool)
{
	constr := &constrs[constr_i]
	
	walk_atom(constr.lhs, constr_i, constrs, updated)
	walk_atom(constr.rhs, constr_i, constrs, updated)

	lhs_has_deps := has_dependencies(constr.lhs^)
	rhs_has_deps := has_dependencies(constr.rhs^)


	if lhs_has_deps && rhs_has_deps {

		#partial switch &lhs in constr.lhs {
		case Atom_Binary:
			#partial switch &rhs in constr.rhs {
			case Atom_Binary:
				if lhs.op == .Add && rhs.op == .Add {

					flatten_mult(&lhs)
					flatten_mult(&rhs)

					lhs_lhs_has_deps := has_dependencies(lhs.lhs^)
					rhs_lhs_has_deps := has_dependencies(rhs.lhs^)

					switch {
					case lhs_lhs_has_deps && rhs_lhs_has_deps:
						lhs.rhs, rhs.lhs = rhs.lhs, lhs.rhs
						atom_neg(lhs.rhs)
						atom_neg(rhs.lhs)
					case lhs_lhs_has_deps:
						lhs.rhs, rhs.rhs = rhs.rhs, lhs.rhs
						atom_neg(lhs.rhs)
						atom_neg(rhs.rhs)
					case rhs_lhs_has_deps:
						lhs.lhs, rhs.lhs = rhs.lhs, lhs.lhs
						atom_neg(lhs.lhs)
						atom_neg(rhs.lhs)
					case:
						lhs.lhs, rhs.rhs = rhs.rhs, lhs.lhs
						atom_neg(lhs.lhs)
						atom_neg(rhs.rhs)
					}

					updated ^= true
				}	
			}
		}

		return
	}


	if rhs_has_deps {
		constr.lhs, constr.rhs = constr.rhs, constr.lhs
	}

	switch_sides: switch &lhs in constr.lhs {
	case Atom_Num:
		// ignore
	case Atom_Var:
		if lhs.f == FRACTION_IDENTITY do break
		
		atom_div(constr.rhs, lhs.f)
		lhs.f = FRACTION_IDENTITY

		updated ^= true
	case Atom_Binary:

		flatten_mult(&lhs)

		new_bin: Atom_Binary
		new_bin.f = FRACTION_IDENTITY

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

			updated ^= true
		}
	case Atom_Binary:
		walk_atom(a.lhs, constr_i, constrs, updated)
		walk_atom(a.rhs, constr_i, constrs, updated)

		// fold multipliers
		flatten_mult(&a)

		lhs_num, is_lhs_num := a.lhs.(Atom_Num)
		rhs_num, is_rhs_num := a.rhs.(Atom_Num)

		if is_lhs_num && is_rhs_num {
			switch a.op {
			case .Add: atom ^= Atom_Num{Fraction{lhs_num.num * rhs_num.den + rhs_num.num * lhs_num.den, lhs_num.den * rhs_num.den}}
			case .Mul: atom ^= Atom_Num{Fraction{lhs_num.num * rhs_num.num, lhs_num.den * rhs_num.den}}
			case .Div: atom ^= Atom_Num{Fraction{lhs_num.num * rhs_num.den, lhs_num.den * rhs_num.num}}
			}

			updated ^= true
			return
		}

		if is_rhs_num {
			switch a.op {
			case .Add:
				// handled above, can only be folded if both sides are numbers
			case .Div:
				atom_div(a.lhs, rhs_num)
				atom ^= a.lhs^

				updated ^= true
				return
			case .Mul:
				atom_mul(a.lhs, rhs_num)
				atom ^= a.lhs^

				updated ^= true
				return
			}
		}

		if is_lhs_num {
			switch a.op {
			case .Add:
				// handled above, can only be folded if both sides are numbers
			case .Div:
				// cannot be folded here
			case .Mul:
				atom_mul(a.rhs, lhs_num)
				atom ^= a.rhs^

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

				updated ^= true	
				return
			case .Mul, .Div:
				// TODO
				// might require squaring
			}
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

atom_mul :: proc (atom: ^Atom, f: Fraction)
{
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
			a.num *= f.num
			a.den *= f.den
		}
	}
}

atom_div :: proc (atom: ^Atom, f: Fraction)
{
	switch f {
	case FRACTION_ZERO:
		panic("division by zero")
	case FRACTION_IDENTITY:
		return
	case:
		switch &a in atom {
		case Atom_Num:
			a.num *= f.den
			a.den *= f.num
		case Atom_Var:
			a.num *= f.den
			a.den *= f.num
		case Atom_Binary:
			a.num *= f.den
			a.den *= f.num
		}
	}
}

atom_neg :: proc (atom: ^Atom)
{
	switch &a in atom {
	case Atom_Num:    a.num = -a.num
	case Atom_Var:    a.num = -a.num
	case Atom_Binary: a.num = -a.num
	}
}

atom_invert :: proc (atom: ^Atom)
{
	switch &a in atom {
	case Atom_Num:    a.den, a.num = a.num, a.den
	case Atom_Var:    a.den, a.num = a.num, a.den
	case Atom_Binary: a.den, a.num = a.num, a.den
	}
}

flatten_mult :: proc (bin: ^Atom_Binary) {
	if bin.f == FRACTION_IDENTITY do return

	switch bin.op {
	case .Add:
		atom_mul(bin.lhs, bin)
		atom_mul(bin.rhs, bin)
	case .Mul:
		atom_mul(bin.lhs, bin)
	case .Div:
		atom_mul(bin.lhs, {bin.num, 1})
		atom_mul(bin.rhs, {bin.den, 1})
	}

	bin.f = FRACTION_IDENTITY
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
