package bilang

import "core:fmt"


Atom :: union #no_nil {
	Atom_Num,
	Atom_Var,
	Atom_Binary,
}

Atom_Num :: struct {
	value: f64,
}

Atom_Var :: struct {
	name: string,
	mult: f64,
}

Atom_Binary :: struct {
	lhs:  ^Atom,
	op:   Binary_Op,
	rhs:  ^Atom,
	mult: f64,
}

Constraint :: struct {
	lhs: ^Atom,
	rhs: ^Atom,
}


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
			mult = 1,
		}
	case ^Expr_Number:
		return Atom_Num{
			value = v.value,
		}
	case ^Expr_Binary:
		return Atom_Binary{
			lhs  = new_atom(walk_expr(v.lhs)),
			op   = v.op,
			rhs  = new_atom(walk_expr(v.rhs)),
			mult = 1,
		}
	case ^Expr_Unary:
		switch v.op {
		case .Neg:
			return Atom_Binary{
				lhs  = new_atom(Atom_Num{}),
				op   = .Sub,
				rhs  = new_atom(walk_expr(v.rhs)),
				mult = 1,
			}
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
	
			if lhs_has_deps && rhs_has_deps do break
	
			if rhs_has_deps {
				constr.lhs, constr.rhs = constr.rhs, constr.lhs
			}

			switch &lhs in constr.lhs {
			case Atom_Num:
				// ignore
			case Atom_Var:
				if lhs.mult == 1 do break
				
				switch &rhs in constr.rhs {
				case Atom_Num:    rhs.value /= lhs.mult
				case Atom_Var:    rhs.mult  /= lhs.mult
				case Atom_Binary: rhs.mult  /= lhs.mult
				}
				lhs.mult = 1

				continue loop
				
			case Atom_Binary:
				lhs_lhs_has_deps := has_dependencies(lhs.lhs^)
				lhs_rhs_has_deps := has_dependencies(lhs.rhs^)
		
				if lhs_lhs_has_deps && lhs_rhs_has_deps do break
		
				new_rhs_op: Atom_Binary
				
				switch lhs.op {
				case .Add, .Mul:
					new_rhs_op.op = lhs.op == .Add ? .Sub : .Div
	
					if !lhs_lhs_has_deps {
						new_rhs_op.rhs = lhs.lhs
						constr.lhs     = lhs.rhs
					} else {
						new_rhs_op.rhs = lhs.rhs
						constr.lhs     = lhs.lhs
					}
				case .Sub, .Div:
					new_rhs_op.op = lhs.op == .Sub ? .Add : .Mul
	
					new_rhs_op.rhs = lhs.rhs
					constr.lhs     = lhs.lhs
				}
	
				new_rhs_op.lhs = constr.rhs
				constr.rhs     = new_atom(new_rhs_op)

				continue loop
			}

			break
		}
	}
}

walk_atom :: proc (atom: ^Atom, constr_i: int, constraints: ^[dynamic]Constraint)
{
	switch a in atom {
	case Atom_Num:
	case Atom_Var:
		for constr, i in constraints {
			if i == constr_i do continue
			if constr.lhs^ == a && a.mult == 1 {
				atom ^= constr.rhs^
			}
			if constr.rhs^ == a && a.mult == 1 {
				atom ^= constr.lhs^
			}
		}
	case Atom_Binary:
		walk_atom(a.lhs, constr_i, constraints)
		walk_atom(a.rhs, constr_i, constraints)

		lhs_num, is_lhs_num := a.lhs.(Atom_Num)
		rhs_num, is_rhs_num := a.rhs.(Atom_Num)

		if is_lhs_num && is_rhs_num {
			switch a.op {
			case .Add: atom ^= Atom_Num{lhs_num.value + rhs_num.value}
			case .Sub: atom ^= Atom_Num{lhs_num.value - rhs_num.value}
			case .Mul: atom ^= Atom_Num{lhs_num.value * rhs_num.value}
			case .Div: atom ^= Atom_Num{lhs_num.value / rhs_num.value}
			}
			break
		}

		if is_rhs_num {
			switch a.op {
			case .Add, .Sub:
				// handled above, can only be folded if both sides are numbers
			case .Div:
				switch &lhs in a.lhs {
				case Atom_Num:    lhs.value /= rhs_num.value
				case Atom_Var:    lhs.mult  /= rhs_num.value
				case Atom_Binary: lhs.mult  /= rhs_num.value
				}
				atom ^= a.lhs^
			case .Mul:
				switch rhs_num.value {
				case 1:
					atom ^= a.lhs^
				case 0:
					atom ^= Atom_Num{0}
				case:
					switch &lhs in a.lhs {
					case Atom_Num:    lhs.value *= rhs_num.value
					case Atom_Var:    lhs.mult  *= rhs_num.value
					case Atom_Binary: lhs.mult  *= rhs_num.value
					}
					atom ^= a.lhs^
				}
			}
			break
		}

		if is_lhs_num {
			switch a.op {
			case .Add, .Sub:
				// handled above, can only be folded if both sides are numbers
			case .Div:
				// cannot be folded here
			case .Mul:
				switch lhs_num.value {
				case 1:
					atom ^= a.rhs^
				case 0:
					atom ^= Atom_Num{0}
				case:
					switch &rhs in a.rhs {
					case Atom_Num:    rhs.value *= lhs_num.value
					case Atom_Var:    rhs.mult  *= lhs_num.value
					case Atom_Binary: rhs.mult  *= lhs_num.value
					}
					atom ^= a.rhs^
				}
			}
			break
		}
	}
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
