package bilang

import "core:fmt"


print_assign :: proc (assign: ^Assign)
{
	print_expr(assign.lhs)
	fmt.print(" \e[0;36m=\e[0m ")
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
	fmt.print("\e[38;5;240m(\e[0m")

	fmt.print("\e[0;36m")
	switch binary.op {
	case .Add: fmt.print("+")
	case .Sub: fmt.print("-")
	case .Mul: fmt.print("*")
	case .Div: fmt.print("/")
	}
	fmt.print("\e[0m")

	fmt.print(" ")
	print_expr(binary.lhs)
	fmt.print(" ")
	print_expr(binary.rhs)
	fmt.print("\e[38;5;240m)\e[0m")
}

print_unary :: proc (unary: ^Unary)
{	
	fmt.print("\e[38;5;240m(\e[0m")
	
	fmt.print("\e[0;36m")
	switch unary.op {
	case .Neg: fmt.print("-")
	case .Pos: fmt.print("+")
	}
	fmt.print("\e[0m")

	fmt.print(" ")
	print_expr(unary.expr)
	fmt.print("\e[38;5;240m)\e[0m")
}

print_ident :: proc (ident: ^Ident)
{
	fmt.print(ident.name)
}

print_number :: proc (number: ^Number)
{
	fmt.print("\e[0;33m")
	fmt.print(number.value)
	fmt.print("\e[0m")
}
