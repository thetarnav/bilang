package bilang

import "core:mem"
import "core:strconv"
import "core:strings"


Decl :: struct {
	lhs     : Expr,
	op_token: Token,
	rhs     : Expr,
}

Expr :: union {
	^Ident,
	^Number,
	^Unary,
	^Binary,
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
	op      : Unary_Op,
	op_token: Token,
	rhs     : Expr,
}

Binary :: struct {
	lhs     : Expr,
	op      : Binary_Op,
	op_token: Token,
	rhs     : Expr,
}

Unary_Op :: enum {
	Pos,
	Neg,
}

Binary_Op :: enum {
	Add,
	Sub,
	Mul,
	Div,
}

token_to_unary_op :: #force_inline proc (kind: Token_Kind) -> (op: Unary_Op, ok: bool)
{
	#partial switch kind {
	case .Add: return .Pos, true
	case .Sub: return .Neg, true
	}
	return
}

token_to_binary_op :: #force_inline proc (kind: Token_Kind) -> (op: Binary_Op, ok: bool)
{
	#partial switch kind {
	case .Add: return .Add, true
	case .Sub: return .Sub, true
	case .Mul: return .Mul, true
	case .Div: return .Div, true
	}
	return
}

precedence_table := [Binary_Op]int {
	.Add = 1,
	.Sub = 1,
	.Mul = 2,
	.Div = 2,
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
) -> (res: []^Decl, err: Parse_Error)
{
	decls := make([dynamic]^Decl, 0, 16, allocator) or_return
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

parse_decl :: proc (p: ^Parser) -> (decl: ^Decl, err: Parse_Error)
{
	decl = new(Decl, p.allocator) or_return

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
parse_expr :: proc (p: ^Parser) -> (expr: Expr, err: Parse_Error) #no_bounds_check
{
	/*
	-a * b + c
	*/

	expr = parse_expr_atom(p) or_return

	binary_block: {
		op := token_to_binary_op(p.token.kind) or_break binary_block
	
		binary := new(Binary, p.allocator) or_return
	
		binary.op       = op
		binary.op_token = p.token
		binary.lhs      = expr
	
		parser_next_token(p)
		binary.rhs = parse_expr_atom(p) or_return
	
		binary_last := binary
		op_last     := op
		
		expr = binary
	
		for {
			op = token_to_binary_op(p.token.kind) or_break binary_block
	
			binary = new(Binary, p.allocator) or_return
	
			binary.op       = op
			binary.op_token = p.token
	
			parser_next_token(p)
			binary.rhs = parse_expr_atom(p) or_return
	
			if precedence_table[op_last] >= precedence_table[op] {
				binary.lhs = expr
				expr       = binary
			} else {
				binary.lhs      = binary_last.rhs
				binary_last.rhs = binary
			}
			
			binary_last = binary
			op_last     = op
		}
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
parse_unary :: proc (p: ^Parser) -> (unary: ^Unary, err: Parse_Error)
{
	op: Unary_Op

	#partial switch p.token.kind {
	case .Add: op = .Pos
	case .Sub: op = .Neg
	case: unreachable()
	}

	unary = new(Unary, p.allocator) or_return

	unary.op = op
	unary.op_token = p.token

	parser_next_token(p)
	unary.rhs = parse_expr_atom(p) or_return

	return
}

@(require_results)
parse_ident :: proc (p: ^Parser) -> (ident: ^Ident, err: Parse_Error)
{
	assert(p.token.kind == .Ident)

	ident = new(Ident, p.allocator) or_return
	ident.name = token_string(p.src, p.token)
	ident.token = p.token

	parser_next_token(p)

	return
}

@(require_results)
parse_number :: proc (p: ^Parser) -> (number: ^Number, err: Parse_Error)
{
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
	case Invalid_Number_Literal_Error:
		text, err = strings.concatenate({
			"Invalid number literal: ",
			display_token_in_line(src, e.token),
		})
	}

	return
}
