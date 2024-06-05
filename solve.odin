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

@(require_results)
solve :: proc (decls: []Decl, allocator := context.allocator) -> []Constraint
{
	context.allocator = allocator

	constraints: [dynamic]Constraint

	for decl in decls {
		constraint: Constraint
		constraint.lhs = new_atom(walk_expr(decl.lhs))
		constraint.rhs = new_atom(walk_expr(decl.rhs))
		append(&constraints, constraint)
	}

	walk_constraints(&constraints)

	return constraints[:]
}

walk_expr :: proc (expr: Expr) -> Atom
{
	switch v in expr {
	case ^Expr_Ident:
		return Atom_Var{
			name = v.name,
			f    = FRACTION_IDENTITY,
		}
	case ^Expr_Number:
		return Atom_Num{
			num = v.value,
			den = 1,
		}
	case ^Expr_Binary:
		bin := Atom_Binary{
			lhs = new_atom(walk_expr(v.lhs)),
			rhs = new_atom(walk_expr(v.rhs)),
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
			rhs := walk_expr(v.rhs)
			atom_neg(&rhs)
			return rhs
		case .Pos:
			return walk_expr(v.rhs)
		}
	}

	return Atom_Num{}
}

walk_constraints :: proc (constraints: ^[dynamic]Constraint)
{
	for &constr, i in constraints {
		loop: for {
			walk_atom(constr.lhs, i, constraints)
			walk_atom(constr.rhs, i, constraints)
	
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

							continue loop
						}	
					}
				}

				break
			}

	
			if rhs_has_deps {
				constr.lhs, constr.rhs = constr.rhs, constr.lhs
			}

			switch &lhs in constr.lhs {
			case Atom_Num:
				// ignore
			case Atom_Var:
				if lhs.f == FRACTION_IDENTITY do break
				
				atom_div(constr.rhs, lhs.f)
				lhs.f = FRACTION_IDENTITY

				continue loop

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
						break loop
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
						break loop
					}
				case .Div:
					new_bin.op  = .Mul
					new_bin.rhs = lhs.rhs
					constr.lhs  = lhs.lhs
				}
	
				new_bin.lhs = constr.rhs
				constr.rhs  = new_atom(new_bin)

				continue loop
			}

			break
		}
	}
}

walk_atom :: proc (atom: ^Atom, constr_i: int, constraints: ^[dynamic]Constraint)
{
	switch &a in atom {
	case Atom_Num:
		// a.num /= a.den
	case Atom_Var:
		for constr, i in constraints {
			if i == constr_i do continue
			if constr.lhs^ == a && a.f == FRACTION_IDENTITY {
				walk_atom(constr.rhs, i, constraints)
				atom ^= atom_copy(constr.rhs^)
			}
			if constr.rhs^ == a && a.f == FRACTION_IDENTITY {
				walk_atom(constr.lhs, i, constraints)
				atom ^= atom_copy(constr.lhs^)
			}
		}
	case Atom_Binary:
		walk_atom(a.lhs, constr_i, constraints)
		walk_atom(a.rhs, constr_i, constraints)

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
			break
		}

		if is_rhs_num {
			switch a.op {
			case .Add:
				// handled above, can only be folded if both sides are numbers
			case .Div:
				atom_div(a.lhs, rhs_num)
				atom ^= a.lhs^
			case .Mul:
				atom_mult(a.lhs, rhs_num)
				atom ^= a.lhs^
			}
			break
		}

		if is_lhs_num {
			switch a.op {
			case .Add:
				// handled above, can only be folded if both sides are numbers
			case .Div:
				// cannot be folded here
			case .Mul:
				atom_mult(a.rhs, lhs_num)
				atom ^= a.rhs^
			}
			break
		}
	}
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

atom_mult :: proc (atom: ^Atom, f: Fraction)
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
		atom_mult(bin.lhs, bin)
		atom_mult(bin.rhs, bin)
	case .Mul:
		atom_mult(bin.lhs, bin)
	case .Div:
		atom_mult(bin.lhs, {bin.num, 1})
		atom_mult(bin.rhs, {1, bin.den})
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
