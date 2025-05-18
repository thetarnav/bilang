package bilang

import "core:mem"
import "core:strconv"
import "core:strings"


Decl :: struct {
	op_token: Token,
	lhs, rhs: Expr,
}

Expr :: union {
	^Expr_Ident,
	^Expr_Number,
	^Expr_Unary,
	^Expr_Binary,
}

Expr_Ident :: struct {
	token: Token,
}

Expr_Number :: struct {
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

token_is_binary :: proc (token: Token) -> bool {
	return tokens_binary[token.kind]
}
token_is_unary :: proc (token: Token) -> bool {
	return tokens_unary[token.kind]
}

token_precedence :: proc (token: Token) -> int {
	return precedence_table[token.kind]
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

		bin := new(Expr_Binary, p.allocator) or_return
		bin.op_token = op
		bin.lhs      = expr
		bin.rhs      = rhs
		expr = bin
	}

	return
}

@(require_results)
parse_expr_atom :: proc (p: ^Parser) -> (expr: Expr, err: Parse_Error)
{
	#partial switch p.token.kind {
	case .Ident:     expr = parse_ident(p)  or_return
	case .Num:       expr = parse_number(p) or_return
	case .Paren_L:   expr = parse_paren(p)  or_return
	case .Add, .Sub: expr = parse_unary(p)  or_return
	case:
		return expr, Unexpected_Token_Error{p.token}
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
	assert(tokens_unary[p.token.kind])

	unary = new(Expr_Unary, p.allocator) or_return
	unary.op_token = p.token

	parser_next_token(p)
	unary.rhs = parse_expr_atom(p) or_return

	return
}

@(require_results)
parse_ident :: proc (p: ^Parser) -> (ident: ^Expr_Ident, err: Parse_Error)
{
	assert(p.token.kind == .Ident)

	ident = new(Expr_Ident, p.allocator) or_return
	ident.token = p.token

	parser_next_token(p)

	return
}

@(require_results)
parse_number :: proc (p: ^Parser) -> (number: ^Expr_Number, err: Parse_Error)
{
	assert(p.token.kind == .Num)

	number = new(Expr_Number, p.allocator) or_return
	number.token = p.token

	parser_next_token(p)

	return
}

expr_number_value :: proc (expr: Expr_Number) -> (value: f64, ok: bool) {
	return strconv.parse_f64(expr.token.text)
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
