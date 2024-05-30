package bilang

Expr :: union {
    Ident,
    Number,
    Unary,
    Binary,
    Paren,
}

Assign :: struct {
	lhs: ^Expr,
	op:  Token,
	rhs: ^Expr,
}

Ident :: struct {
    token: Token,
    name : string,
}

Number :: struct {
    token: Token,
    value: f32,
}

Unary :: struct {
	op:   Token,
	expr: ^Expr,
}

Binary :: struct {
    left : ^Expr,
    op   : Token,
    right: ^Expr,
}

Paren :: struct {
	open : Token,
	expr : ^Expr,
	close: Token,
}


