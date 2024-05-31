package bilang

import "core:mem"
import "core:strconv"
import "core:strings"


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
parser_next_token_expect :: proc(
	t: ^Tokenizer,
	expected: Token_Kind,
) -> (token: Token, err: Parse_Error) {
	ok: bool
	token, ok = next_token_expect(t, expected)
	if !ok {
		err = Unexpected_Token_Error{token}
	}
	return
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
		case .EOL:
			continue
		case .EOF:
			return
		case:
			expr := parse_expr(&t, token, allocator) or_return
			append(&decls, new_expr(expr, allocator) or_return) or_return
		}

		token = parser_next_token_expect(&t, .Eq) or_return
		token = next_token(&t)
		expr := parse_expr(&t, token, allocator) or_return
		append(&decls, new_expr(expr, allocator) or_return) or_return
	}

	return
}

new_expr :: proc (
	expr: Expr,
	allocator: mem.Allocator,
) -> (ptr: ^Expr, err: Allocator_Error) {
	ptr  = new(Expr, allocator) or_return
	ptr ^= expr
	return
}

@(require_results)
parse_expr :: proc (
	t: ^Tokenizer,
	token: Token,
	allocator: mem.Allocator,
) -> (expr: Expr, err: Parse_Error) {

	#partial switch token.kind {
	case .Ident:     expr      = parse_ident(t, token)
	case .Num:       expr, err = parse_number(t, token)
	case .Paren_L:   expr, err = parse_paren(t, token, allocator)
	case .Add, .Sub: expr, err = parse_unary(t, token, allocator)
	case:
		err = Unexpected_Token_Error{token}
	}

	return
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
	allocator: mem.Allocator,
) -> (paren: Paren, err: Parse_Error) {
	assert(open.kind == .Paren_L)
	
	expr  := parse_expr(t, next_token(t), allocator) or_return
	close := parser_next_token_expect(t, .Paren_R) or_return
	paren  = Paren{
		open,
		new_expr(expr, allocator) or_return,
		close,
	}

	return
}

parse_unary :: proc (
	t: ^Tokenizer,
	op: Token,
	allocator: mem.Allocator,
) -> (unary: Unary, err: Parse_Error) {
	assert(op.kind == .Add || op.kind == .Sub)
	
	expr := parse_expr(t, next_token(t), allocator) or_return
	unary = Unary{
		op,
		new_expr(expr, allocator) or_return,
	}

	return
}

/*
Return a pretty string representation of a parser error.
*/
@(require_results)
parser_error_to_string :: proc(
	src: string,
	parser_err: Parse_Error,
	allocator := context.allocator,
) -> (text: string, err: mem.Allocator_Error) #optional_allocator_error {
	context.allocator = allocator

	switch e in parser_err {
	case Allocator_Error:
		switch e {
		case .None:                 text = "Allocator_Error: None"
		case .Out_Of_Memory:        text = "Allocator_Error: Out of memory"
		case .Invalid_Pointer:      text = "Allocator_Error: Invalid pointer"
		case .Invalid_Argument:     text = "Allocator_Error: Invalid argument"
		case .Mode_Not_Implemented: text = "Allocator_Error: Mode not implemented"
		}
	case Unexpected_Token_Error:
		text, err = strings.concatenate({
			"Unexpected token: ",
			display_token_in_line(src, e.token),
		})
	case Invalid_Number_Literal_Error:
		text, err = strings.concatenate({
			"Invalid number literal: ",
			display_token_in_line(src, e.token),
		})
	}

	return
}
