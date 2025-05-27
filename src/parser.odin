package bilang

import "core:mem"
import "core:strconv"
import "core:strings"


Decl :: struct {
	op_token: Token,
	lhs, rhs: Expr,
}

Expr :: union {
	^Expr_Single,
	^Expr_Unary,
	^Expr_Binary,
}

Expr_Single :: struct {
	token: Token,
}

Expr_Unary :: struct {
	op_token: Token,
	rhs     : Expr,
}

Expr_Binary :: struct {
	op_token: Token,
	lhs, rhs: Expr,
}

tokens_unary := #partial [Token_Kind]bool{
	.Add = true,
	.Sub = true,
}

tokens_binary := #partial [Token_Kind]bool{
	.Add = true,
	.Sub = true,
	.Mul = true,
	.Div = true,
	.Pow = true,
}

precedence_table := #partial [Token_Kind]int{
	.Add = 1,
	.Sub = 1,
	.Mul = 2,
	.Div = 2,
	.Pow = 3,
}

Parse_Error :: union {
	Allocator_Error,
	Unexpected_Token_Error,
}

Allocator_Error :: mem.Allocator_Error

Unexpected_Token_Error :: struct {
	token: Token,
}

Parser :: struct {
	src:       string,
	t:         Tokenizer,
	token:     Token,
	allocator: mem.Allocator,
}

@require_results
token_is_binary :: proc (token: Token) -> bool {
	return tokens_binary[token.kind]
}
@require_results
token_is_unary :: proc (token: Token) -> bool {
	return tokens_unary[token.kind]
}
@require_results
token_precedence :: proc (token: Token) -> int {
	return precedence_table[token.kind]
}

@require_results
expr_single_new :: proc (token: Token, allocator := context.allocator, loc := #caller_location) ->
	(value: ^Expr_Single, err: Parse_Error)
{
	assert(token.kind == .Ident || token.kind == .Float || token.kind == .Int || token.kind == .Str, loc=loc)

	value = new(Expr_Single, allocator, loc=loc) or_return
	value.token = token

	return
}
@require_results
expr_unary_new :: proc (op: Token, rhs: Expr, allocator := context.allocator, loc := #caller_location) ->
	(unary: ^Expr_Unary, err: Parse_Error)
{
	assert(token_is_unary(op), loc=loc)

	unary = new(Expr_Unary, allocator, loc=loc) or_return
	unary^ = {op, rhs}

	return
}
@require_results
expr_binary_new :: proc (op: Token, lhs, rhs: Expr, allocator: mem.Allocator, loc := #caller_location) ->
	(bin: ^Expr_Binary, err: Parse_Error)
{
	assert(token_is_binary(op), loc=loc)

	bin = new(Expr_Binary, allocator, loc=loc) or_return
	bin^ = {op, lhs, rhs}

	return
}

parser_next_token :: proc(p: ^Parser)
{
	p.token = next_token(&p.t)
}

parser_next_token_expect :: proc(
	p: ^Parser,
	expected: Token_Kind,
) -> (err: Parse_Error)
{
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
) -> (err: Parse_Error)
{
	if p.token.kind != expected {
		err = Unexpected_Token_Error{p.token}
	}
	return
}

@(require_results)
parse_src :: proc (
	src: string,
	allocator := context.allocator,
) -> (res: []Decl, err: Parse_Error)
{
	decls := make([dynamic]Decl, 0, 16, allocator) or_return
	defer shrink(&decls)

	p: Parser = {
		src       = src,
		t         = make_tokenizer(src),
		allocator = allocator,
	}
	parser_next_token(&p)
	
	loop: for {
		#partial switch p.token.kind {
		case .EOL:
			parser_next_token(&p)
			continue
		case .EOF:
			break loop
		case:
			decl := parse_decl(&p) or_return
			append(&decls, decl) or_return
		}
	}

	res = decls[:]
	return
}

parse_decl :: proc (p: ^Parser) -> (decl: Decl, err: Parse_Error)
{
	decl.lhs = parse_expr(p) or_return
	
	parser_curr_token_expect(p, .Eq) or_return
	decl.op_token = p.token

	parser_next_token(p)
	decl.rhs = parse_expr(p) or_return

	#partial switch p.token.kind {
	case .EOL, .EOF:
		// good
	case:
		err = Unexpected_Token_Error{p.token}
	}

	parser_next_token(p)

	return
}

@(require_results)
parse_expr :: proc (p: ^Parser) -> (expr: Expr, err: Parse_Error) {
	return parse_expr_bp(p, 1)
}

@(require_results)
parse_expr_bp :: proc (p: ^Parser, min_bp: int) -> (expr: Expr, err: Parse_Error) #no_bounds_check
{
	/*
	-a * b + c
	*/

	expr = parse_expr_atom(p) or_return

	for {
		op := p.token
		token_is_binary(op) or_break

		lbp := token_precedence(op)
		if lbp < min_bp do return

		rbp := lbp
		if op.kind != .Pow {
			rbp += 1 // Right-associative for Pow
		}

		parser_next_token(p)

		rhs := parse_expr_bp(p, rbp) or_return

		expr = expr_binary_new(op, expr, rhs, p.allocator) or_return
	}

	return
}

@(require_results)
parse_expr_atom :: proc (p: ^Parser) -> (expr: Expr, err: Parse_Error)
{
	#partial switch p.token.kind {
	case .Ident, .Float, .Int, .Str: expr = parse_single(p) or_return
	case .Paren_L:                      expr = parse_paren(p)  or_return
	case .Add, .Sub:                    expr = parse_unary(p)  or_return
	case:
		err = Unexpected_Token_Error{p.token}
	}

	return
}

@(require_results)
parse_paren :: proc (p: ^Parser) -> (expr: Expr, err: Parse_Error)
{
	assert(p.token.kind == .Paren_L)

	parser_next_token(p)
	expr = parse_expr(p) or_return

	parser_curr_token_expect(p, .Paren_R) or_return

	parser_next_token(p)

	return
}

@(require_results)
parse_unary :: proc (p: ^Parser) -> (unary: ^Expr_Unary, err: Parse_Error)
{
	op := p.token
	parser_next_token(p)
	rhs := parse_expr_atom(p) or_return
	return expr_unary_new(op, rhs, p.allocator)
}

@(require_results)
parse_single :: proc (p: ^Parser) -> (expr: ^Expr_Single, err: Parse_Error)
{
	expr, err = expr_single_new(p.token, p.allocator)
	parser_next_token(p)
	return
}

expr_single_float_value :: proc (expr: Expr_Single) -> (value: f64, ok: bool) {
	assert(expr.token.kind == .Float)
	return strconv.parse_f64(expr.token.text)
}
expr_single_int_value :: proc (expr: Expr_Single) -> (value: int, ok: bool) {
	assert(expr.token.kind == .Int)
	return strconv.parse_int(expr.token.text)
}
expr_single_ident_value :: proc (expr: Expr_Single) -> (value: string) {
	assert(expr.token.kind == .Ident)
	return expr.token.text
}
expr_single_string_value :: proc (expr: Expr_Single, allocator := context.allocator) -> (value: string, ok: bool) {
	assert(expr.token.kind == .Str)
	allocated: bool
	value, allocated, ok = strconv.unquote_string(expr.token.text, allocator)
	return
}

/*
Return a pretty string representation of a parser error.
*/
@(require_results)
parser_error_to_string :: proc (
	src: string,
	parser_err: Parse_Error,
	allocator := context.allocator,
) -> (text: string, err: mem.Allocator_Error) #optional_allocator_error
{
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
	}

	return
}
