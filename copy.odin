package bilang


@(require_results)
copy_decl :: proc (decl: Decl, allocator := context.allocator) -> (new_decl: ^Decl)
{
	new_decl = new(Decl, allocator)
	new_decl ^= decl

	new_decl.lhs = copy_expr(decl.lhs, allocator)
	new_decl.rhs = copy_expr(decl.rhs, allocator)

	return
}

@(require_results)
copy_expr :: proc (expr: Expr, allocator := context.allocator) -> (new_expr: Expr)
{
	switch v in expr {
	case ^Ident:  new_expr = copy_ident (v^, allocator)
	case ^Number: new_expr = copy_number(v^, allocator)
	case ^Unary:  new_expr = copy_unary (v^, allocator)
	case ^Binary: new_expr = copy_binary(v^, allocator)
	}
	return
}

@(require_results)
copy_ident :: proc (ident: Ident, allocator := context.allocator) -> (new_ident: ^Ident)
{
	new_ident = new(Ident, allocator)
	new_ident ^= ident
	return
}

@(require_results)
copy_number :: proc (number: Number, allocator := context.allocator) -> (new_number: ^Number)
{
	new_number = new(Number, allocator)
	new_number ^= number
	return
}

@(require_results)
copy_unary :: proc (unary: Unary, allocator := context.allocator) -> (new_unary: ^Unary)
{
	new_unary = new(Unary, allocator)
	new_unary ^= unary

	new_unary.rhs = copy_expr(unary.rhs, allocator)

	return
}

@(require_results)
copy_binary :: proc (binary: Binary, allocator := context.allocator) -> (new_binary: ^Binary)
{
	new_binary = new(Binary, allocator)
	new_binary ^= binary

	new_binary.lhs = copy_expr(binary.lhs, allocator)
	new_binary.rhs = copy_expr(binary.rhs, allocator)

	return
}
