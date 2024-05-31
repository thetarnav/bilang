package bilang

import "core:fmt"
import "core:mem"
import "core:strconv"


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

Parse_Error :: union {
	Allocator_Error,
	Unexpected_Token_Error,
	Invalid_Number_Literal_Error,
}

Allocator_Error :: mem.Allocator_Error

Unexpected_Token_Error :: struct {
	token: Token,
}

Invalid_Number_Literal_Error :: struct {
	token: Token,
}


@(require_results)
parse_file :: proc (
	file: string,
	allocator := context.allocator,
) -> (res: []^Expr, err: Parse_Error) {

	decls := make([dynamic]^Expr, 0, 16, allocator) or_return
	defer {
		res = decls[:]
		shrink(&decls)
	}

	t := make_tokenizer(file)
	
	for {
		token := next_token(&t)

		#partial switch token.kind {
		case .Comment:
			continue
		case .EOF:
			return
		case .Ident:
		case .Num:
		case .Paren_L:
		case .Add:
		case .Sub:
		case:
			err = Unexpected_Token_Error{token}
			return
		}
	}

	return
}

@(require_results)
parse_expr :: proc (
	t: ^Tokenizer,
) -> (res: ^Expr, err: Parse_Error) {

	token := next_token(t)

	#partial switch token.kind {
	case .Ident:
		return parse_ident(t, token)
	case .Num:
		return parse_number(t, token)
	case .Paren_L:
		return parse_paren(t, token)
	case .Add, .Sub:
		return parse_unary(t, token)
	case:
		return nil, Unexpected_Token_Error{token}
	}
}

parse_ident :: proc (
	t: ^Tokenizer,
	token: Token,
) -> (ident: Ident) {
	assert(token.kind == .Ident)
	
	ident.name = token_string(t.src, token)
	ident.token = token

	return
}

parse_number :: proc (
	t: ^Tokenizer,
	token: Token,
) -> (number: Number, err: Parse_Error) {
	assert(token.kind == .Num)
	
	value, ok := strconv.parse_f32(token_string(t.src, token))
	if !ok {
		err = Invalid_Number_Literal_Error{token}
		return
	}

	number.value = value
	number.token = token

	return
}

parse_paren :: proc (
	t: ^Tokenizer,
	open: Token,
) -> (res: ^Expr, err: Parse_Error) {
	
	expr, err := parse_expr(t)
	if err != nil {
		return nil, err
	}

	close := next_token(t)
	if close.kind != .Paren_R {
		return nil, Unexpected_Token_Error{close}
	}

	paren := Paren{open, expr, close}
	return &paren, nil
}

parse_unary :: proc (
	t: ^Tokenizer,
	op: Token,
) -> (res: ^Expr, err: Parse_Error) {
	
	expr, err := parse_expr(t)
	if err != nil {
		return nil, err
	}

	unary := Unary{op, expr}
	return &unary, nil
}
