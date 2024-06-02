package bilang

import "core:fmt"
import "core:os"

Atom :: union #no_nil {
	f32,
	^Operation,
	string,
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

	op_ptr := new(Operation)
	op_ptr ^= op

	return op_ptr
}
