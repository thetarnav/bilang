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

	fmt.printfln("\n------\n")
	print_contraints(constraints)

	fmt.printfln("\n------\n")
	walk_constraints(&constraints)
	print_contraints(constraints)

	fmt.printfln("\n------\n")
	walk_constraints(&constraints)
	print_contraints(constraints)
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
		constr.lhs = walk_atom(constr.lhs, i, constraints)
		constr.rhs = walk_atom(constr.rhs, i, constraints)

		l_has_deps := has_dependencies(constr.lhs)
		r_has_deps := has_dependencies(constr.rhs)

		if l_has_deps && r_has_deps do continue

		if l_has_deps {
			constr.lhs, constr.rhs = constr.rhs, constr.lhs
		}

		if op, is_op := constr.rhs.(^Operation); is_op {

			op_l_has_deps := has_dependencies(op.lhs)
			op_r_has_deps := has_dependencies(op.rhs)

			if op_l_has_deps && op_r_has_deps do continue

			op_ptr := new(Operation)

			if !op_l_has_deps {
				op_ptr.rhs = op.lhs
				constr.rhs = op.rhs
			} else {
				op_ptr.rhs = op.rhs
				constr.rhs = op.lhs
			}
			op_ptr.lhs = constr.lhs
			constr.lhs = op_ptr

			switch op.op {
			case .Add: op_ptr.op = .Sub
			case .Sub: op_ptr.op = .Add
			case .Mul: op_ptr.op = .Div
			case .Div: op_ptr.op = .Mul
			}
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

print_contraints :: proc (constraints: [dynamic]Constraint)
{
	for constr in constraints {
		print_atom(constr.lhs)
		fmt.printf(" = ")
		print_atom(constr.rhs)
		fmt.printf("\n")
	}
}

print_atom :: proc (atom: Atom)
{
	switch a in atom {
	case f32:
		fmt.printf("%f", a)
	case string:
		fmt.printf("%s", a)
	case ^Operation:
		fmt.printf("(")
		print_atom(a.lhs)
		switch a.op {
		case .Add: fmt.printf(" + ")
		case .Sub: fmt.printf(" - ")
		case .Mul: fmt.printf(" * ")
		case .Div: fmt.printf(" / ")
		}
		print_atom(a.rhs)
		fmt.printf(")")
	}
}
