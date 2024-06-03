package bilang


Atom :: union #no_nil {
	f32,
	string,
	^Operation,
}

Operation :: struct {
	lhs: Atom,
	op:  Binary_Op,
	rhs: Atom,
}

Constraint :: struct {
	lhs: Atom,
	rhs: Atom,
}


@(require_results)
solve :: proc (decls: []^Decl, allocator := context.allocator) -> []Constraint
{
	context.allocator = allocator

	constraints: [dynamic]Constraint

	for decl in decls {
		constraint: Constraint
		constraint.lhs = walk_expr(decl.lhs)
		constraint.rhs = walk_expr(decl.rhs)
		append(&constraints, constraint)
	}

	walk_constraints(&constraints)

	return constraints[:]
}

walk_expr :: proc (expr: Expr) -> Atom
{
	op: Operation

	switch v in expr {
	case ^Ident:
		return v.name
	case ^Number:
		return v.value
	case ^Binary:
		op.lhs = walk_expr(v.lhs)
		op.rhs = walk_expr(v.rhs)
		op.op = v.op
	case ^Unary:
		switch v.op {
		case .Neg:
			op.rhs = walk_expr(v.rhs)
			op.op = .Sub
		case .Pos:
			return walk_expr(v.rhs)
		}
	}

	lnum, is_l_num := op.lhs.(f32)
	rnum, is_r_num := op.rhs.(f32)

	if is_l_num && is_r_num {
		switch op.op {
		case .Add: return lnum + rnum
		case .Sub: return lnum - rnum
		case .Mul: return lnum * rnum
		case .Div: return lnum / rnum
		}
	}

	op_ptr := new(Operation)
	op_ptr ^= op

	return op_ptr
}

walk_constraints :: proc (constraints: ^[dynamic]Constraint)
{
	for &constr, i in constraints {
		for {
			constr.lhs = walk_atom(constr.lhs, i, constraints)
			constr.rhs = walk_atom(constr.rhs, i, constraints)
	
			lhs_has_deps := has_dependencies(constr.lhs)
			rhs_has_deps := has_dependencies(constr.rhs)
	
			if lhs_has_deps && rhs_has_deps do break
	
			if rhs_has_deps {
				constr.lhs, constr.rhs = constr.rhs, constr.lhs
			}
	
			lhs := constr.lhs.(^Operation) or_break
	
			lhs_lhs_has_deps := has_dependencies(lhs.lhs)
			lhs_rhs_has_deps := has_dependencies(lhs.rhs)
	
			if lhs_lhs_has_deps && lhs_rhs_has_deps do break
	
			new_rhs_op := new(Operation)
			
			switch lhs.op {
			case .Add, .Div:
				new_rhs_op.op = lhs.op == .Add ? .Sub : .Mul

				if !lhs_lhs_has_deps {
					new_rhs_op.rhs = lhs.lhs
					constr.lhs     = lhs.rhs
				} else {
					new_rhs_op.rhs = lhs.rhs
					constr.lhs     = lhs.lhs
				}
			case .Sub, .Mul:
				new_rhs_op.op = lhs.op == .Sub ? .Add : .Div

				new_rhs_op.rhs = lhs.rhs
				constr.lhs     = lhs.lhs
			}

			new_rhs_op.lhs = constr.rhs
			constr.rhs     = new_rhs_op
		}
	}
}

walk_atom :: proc (atom: Atom, constr_i: int, constraints: ^[dynamic]Constraint) -> Atom
{
	switch a in atom {
	case f32:
	case string:
		for constr, i in constraints {
			if i == constr_i do continue
			if constr.lhs == a {
				return constr.rhs
			}
			if constr.rhs == a {
				return constr.lhs
			}
		}
	case ^Operation:
		a.lhs = walk_atom(a.lhs, constr_i, constraints)
		a.rhs = walk_atom(a.rhs, constr_i, constraints)

		lnum, is_l_num := a.lhs.(f32)
		rnum, is_r_num := a.rhs.(f32)

		if is_l_num && is_r_num {
			switch a.op {
			case .Add: return lnum + rnum
			case .Sub: return lnum - rnum
			case .Mul: return lnum * rnum
			case .Div: return lnum / rnum
			}
		}
	}
	return atom
}

has_dependencies :: proc (atom: Atom) -> bool
{
	switch a in atom {
	case f32:
	case string:
		return true
	case ^Operation:
		return has_dependencies(a.lhs) || has_dependencies(a.rhs)
	}
	return false
}
