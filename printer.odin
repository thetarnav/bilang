package bilang

import "core:fmt"


print_assign :: proc (assign: ^Assign)
{
	print_expr(assign.lhs)
	fmt.print(" = ")
	print_expr(assign.rhs)
	fmt.print("\n")
}

print_expr :: proc (expr: Expr)
{
	switch v in expr {
	case ^Binary: print_binary(v)
	case ^Unary:  print_unary (v)
	case ^Ident:  print_ident (v)
	case ^Number: print_number(v)
	}

	return
}

print_binary :: proc (binary: ^Binary)
{
	fmt.print("(")
	fmt.print(binary.op)
	fmt.print(" ")
	print_expr(binary.lhs)
	fmt.print(" ")
	print_expr(binary.rhs)
	fmt.print(")")
}

print_unary :: proc (unary: ^Unary)
{	
	fmt.print("(")
	fmt.print(unary.op)
	fmt.print(" ")
	print_expr(unary.expr)
	fmt.print(")")
}

print_ident :: proc (ident: ^Ident)
{
	fmt.print(ident.name)
}

print_number :: proc (number: ^Number)
{
	fmt.print(number.value)
}
