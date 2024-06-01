package bilang

import "core:mem"
import "core:strconv"
import "core:strings"


Expr :: union {
	^Ident,
	^Number,
	^Unary,
	^Binary,
	^Paren,
}

Assign :: struct {
	lhs: Expr,
	op : Token,
	rhs: Expr,
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
	expr: Expr,
}

Binary :: struct {
	lhs: Expr,
	op : Token,
	rhs: Expr,
}

Paren :: struct {
	open : Token,
	expr : Expr,
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

Parser :: struct {
	src:       string,
	t:         Tokenizer,
	token:     Token,
	allocator: mem.Allocator,
}

parser_next_token :: proc(p: ^Parser) {
	p.token = next_token(&p.t)
}

parser_next_token_expect :: proc(
	p: ^Parser,
	expected: Token_Kind,
) -> (err: Parse_Error) {
	ok: bool
	p.token, ok = next_token_expect(&p.t, expected)
	if !ok {
		err = Unexpected_Token_Error{p.token}
	}
	return
}

parser_curr_token_expect :: proc(
	p: ^Parser,
	expected: Token_Kind,
) -> (err: Parse_Error) {
	if p.token.kind != expected {
		err = Unexpected_Token_Error{p.token}
	}
	return
}

@(require_results)
parse_file :: proc (
	src: string,
	allocator := context.allocator,
) -> (res: []Expr, err: Parse_Error) {

	decls := make([dynamic]Expr, 0, 16, allocator) or_return
	defer shrink(&decls)

	p: Parser = {
		src       = src,
		t         = make_tokenizer(src),
		allocator = allocator,
	}
	
	loop: for {
		parser_next_token(&p)

		#partial switch p.token.kind {
		case .EOL:
			continue
		case .EOF:
			break loop
		case:
			expr := parse_expr(&p) or_return
			append(&decls, expr) or_return
		}

		parser_curr_token_expect(&p, .Eq) or_return

		parser_next_token(&p)
		expr := parse_expr(&p) or_return
		append(&decls, expr) or_return

		parser_curr_token_expect(&p, .EOL) or_return
	}

	res = decls[:]
	return
}

@(require_results)
parse_expr :: proc (p: ^Parser) -> (expr: Expr, err: Parse_Error) {

	/*
	-a * b + c
	*/

	#partial switch p.token.kind {
	case .Ident:     expr = parse_ident(p)  or_return
	case .Num:       expr = parse_number(p) or_return
	case .Paren_L:   expr = parse_paren(p)  or_return
	case .Add, .Sub: expr = parse_unary(p)  or_return
	case:
		return expr, Unexpected_Token_Error{p.token}
	}

	#partial switch p.token.kind {
	case .Add, .Sub, .Mul, .Div:
		binary := new(Binary, p.allocator) or_return
		binary.op  = p.token
		binary.lhs = expr
		parser_next_token(p)
		binary.rhs = parse_expr(p) or_return
		expr = binary
	}

	return
}

@(require_results)
parse_ident :: proc (p: ^Parser) -> (ident: ^Ident, err: Parse_Error) {
	assert(p.token.kind == .Ident)

	ident = new(Ident, p.allocator) or_return
	ident.name = token_string(p.src, p.token)
	ident.token = p.token

	parser_next_token(p)

	return
}

@(require_results)
parse_number :: proc (p: ^Parser) -> (number: ^Number, err: Parse_Error) {
	assert(p.token.kind == .Num)

	number = new(Number, p.allocator) or_return
	
	value, ok := strconv.parse_f32(token_string(p.src, p.token))
	if !ok {
		err = Invalid_Number_Literal_Error{p.token}
	}

	number.value = value
	number.token = p.token

	parser_next_token(p)

	return
}

@(require_results)
parse_paren :: proc (p: ^Parser) -> (paren: ^Paren, err: Parse_Error) {
	assert(p.token.kind == .Paren_L)

	paren = new(Paren, p.allocator) or_return

	paren.open = p.token
	parser_next_token(p)
	paren.expr = parse_expr(p) or_return
	parser_curr_token_expect(p, .Paren_R) or_return
	paren.close = p.token

	parser_next_token(p)

	return
}

@(require_results)
parse_unary :: proc (p: ^Parser) -> (unary: ^Unary, err: Parse_Error) {
	assert(p.token.kind == .Add || p.token.kind == .Sub)

	unary = new(Unary, p.allocator) or_return

	unary.op = p.token
	parser_next_token(p)
	unary.expr = parse_expr(p) or_return

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
		case .None:                 unreachable()
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
