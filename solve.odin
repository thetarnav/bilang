package bilang

import "core:fmt"
import "core:log"


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
								atom_mult(lhs.rhs, -1)
								atom_mult(rhs.lhs, -1)
							case lhs_lhs_has_deps:
								lhs.rhs, rhs.rhs = rhs.rhs, lhs.rhs
								atom_mult(lhs.rhs, -1)
								atom_mult(rhs.rhs, -1)
							case rhs_lhs_has_deps:
								lhs.lhs, rhs.lhs = rhs.lhs, lhs.lhs
								atom_mult(lhs.lhs, -1)
								atom_mult(rhs.lhs, -1)
							case:
								lhs.lhs, rhs.rhs = rhs.rhs, lhs.lhs
								atom_mult(lhs.lhs, -1)
								atom_mult(rhs.rhs, -1)
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
				if lhs.mult == 1 do break
				
				atom_mult(constr.rhs, 1 / lhs.mult)
				lhs.mult = 1

				continue loop

			case Atom_Binary:

				flatten_mult(&lhs)

				new_bin: Atom_Binary
				new_bin.mult = 1
				
				switch lhs.op {
				case .Add, .Mul:
					new_bin.op = lhs.op == .Add ? .Sub : .Div

					if !has_dependencies(lhs.lhs^)
					{
						new_bin.rhs = lhs.lhs
						constr.lhs  = lhs.rhs
					}
					else if !has_dependencies(lhs.rhs^)
					{
						new_bin.rhs = lhs.rhs
						constr.lhs  = lhs.lhs
					}
					else do break loop
					
				case .Sub, .Div:
					new_bin.op = lhs.op == .Sub ? .Add : .Mul
	
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

		// fold multipliers
		flatten_mult(&a)

		if is_rhs_num {
			switch a.op {
			case .Add, .Sub:
				// handled above, can only be folded if both sides are numbers
			case .Div:
				atom_mult(a.lhs, 1 / rhs_num.value)
				atom ^= a.lhs^
			case .Mul:
				switch rhs_num.value {
				case 1:
					atom ^= a.lhs^
				case 0:
					atom ^= Atom_Num{0}
				case:
					atom_mult(a.lhs, rhs_num.value)
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
					atom_mult(a.rhs, lhs_num.value)
					atom ^= a.rhs^
				}
			}
			break
		}
	}
}

atom_mult :: proc (atom: ^Atom, mult: f64)
{
	switch &a in atom {
	case Atom_Num:    a.value *= mult
	case Atom_Var:    a.mult  *= mult
	case Atom_Binary: a.mult  *= mult
	}
}

flatten_mult :: proc (bin: ^Atom_Binary) {
	if bin.mult == 1 do return

	switch bin.op {
	case .Add, .Sub:
		atom_mult(bin.lhs, bin.mult)
		atom_mult(bin.rhs, bin.mult)
	case .Mul, .Div:
		atom_mult(bin.lhs, bin.mult)
	}

	bin.mult = 1
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
