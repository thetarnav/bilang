package bilang

import "core:fmt"
import "core:os"


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


main :: proc ()
{
	language_input := `
a + b = 10
a     = -4 + 2
`

	decls, err := parse_src(language_input)

	print_decls(decls)

	if err != nil {
		fmt.print(parser_error_to_string(language_input, err))
		os.exit(1)
	}

	constraints: [dynamic]Constraint

	for decl in decls {
		constraint: Constraint
		constraint.lhs = walk_expr(decl.lhs)
		constraint.rhs = walk_expr(decl.rhs)
		append(&constraints, constraint)
	}

	for constraint in constraints {
		fmt.printfln("%#v\n=\n%#v\n", constraint.lhs, constraint.rhs)
	}

	fmt.printfln("\n------\n")
	
	walk_constraints(&constraints)

	for constraint in constraints {
		fmt.printfln("%#v\n=\n%#v\n", constraint.lhs, constraint.rhs)
	}

	fmt.printfln("\n------\n")
	
	walk_constraints(&constraints)

	for constraint in constraints {
		fmt.printfln("%#v\n=\n%#v\n", constraint.lhs, constraint.rhs)
	}
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
			op.lhs = walk_expr(v.rhs)
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

walk_constraints :: proc (constraints: ^[dynamic]Constraint) {
	for &constr, i in constraints {
		constr.lhs = walk_atom(constr.lhs, i, constraints)
		constr.rhs = walk_atom(constr.rhs, i, constraints)
	}
}

walk_atom :: proc (atom: Atom, constr_i: int, constraints: ^[dynamic]Constraint) -> Atom {
	switch a in atom {
	case f32:
	case string:
		for constr, i in constraints {
			if i == constr_i do continue
			if s, is_string := constr.lhs.(string); is_string && s == a {
				return constr.rhs
			}
			if s, is_string := constr.rhs.(string); is_string && s == a {
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
