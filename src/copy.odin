package bilang


@(require_results)
copy_expr :: proc (expr: Expr, allocator := context.allocator) -> (new_expr: Expr)
{
	switch v in expr {
	case ^Expr_Single: new_expr = copy_single(v^, allocator)
	case ^Expr_Unary:  new_expr = copy_unary (v^, allocator)
	case ^Expr_Binary: new_expr = copy_binary(v^, allocator)
	}
	return
}

@(require_results)
copy_single :: proc (single: Expr_Single, allocator := context.allocator) -> (new_single: ^Expr_Single)
{
	new_single = new(Expr_Single, allocator)
	new_single ^= single
	return
}

@(require_results)
copy_unary :: proc (unary: Expr_Unary, allocator := context.allocator) -> (new_unary: ^Expr_Unary)
{
	new_unary = new(Expr_Unary, allocator)
	new_unary ^= unary

	new_unary.rhs = copy_expr(unary.rhs, allocator)

	return
}

@(require_results)
copy_binary :: proc (binary: Expr_Binary, allocator := context.allocator) -> (new_binary: ^Expr_Binary)
{
	new_binary = new(Expr_Binary, allocator)
	new_binary ^= binary

	new_binary.lhs = copy_expr(binary.lhs, allocator)
	new_binary.rhs = copy_expr(binary.rhs, allocator)

	return
}
