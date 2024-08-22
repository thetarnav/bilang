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
	^Expr_Ident,
	^Expr_Number,
	^Expr_Unary,
	^Expr_Binary,
}

Expr_Ident :: struct {
	token: Token,
	name : string,
}

Expr_Number :: struct {
	token: Token,
	value: f64,
}

Expr_Unary :: struct {
	op      : Expr_Unary_Op,
	op_token: Token,
	rhs     : Expr,
}

Expr_Binary :: struct {
	lhs     : Expr,
	op      : Expr_Binary_Op,
	op_token: Token,
	rhs     : Expr,
}

Expr_Unary_Op :: enum {
	Pos,
	Neg,
}

Expr_Binary_Op :: enum {
	Add,
	Sub,
	Mul,
	Div,
	Pow,
}

token_to_unary_op :: #force_inline proc (kind: Token_Kind) -> (op: Expr_Unary_Op, ok: bool)
{
	#partial switch kind {
	case .Add: return .Pos, true
	case .Sub: return .Neg, true
	}
	return
}

token_to_binary_op := #force_inline proc (kind: Token_Kind) -> (op: Expr_Binary_Op, ok: bool)
{
	#partial switch kind {
	case .Add: return .Add, true
	case .Sub: return .Sub, true
	case .Mul: return .Mul, true
	case .Div: return .Div, true
	case .Pow: return .Pow, true
	}
	return
}

precedence_table := [Expr_Binary_Op]int {
	.Add = 1,
	.Sub = 1,
	.Mul = 2,
	.Div = 2,
	.Pow = 3,
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
parse_expr :: proc (p: ^Parser) -> (expr: Expr, err: Parse_Error) #no_bounds_check
{
	/*
	-a * b + c
	*/

	expr = parse_expr_atom(p) or_return

	op, is_bin_op := token_to_binary_op(p.token.kind)
	if !is_bin_op do return

	bin := new(Expr_Binary, p.allocator) or_return

	bin.op       = op
	bin.op_token = p.token
	bin.lhs      = expr

	parser_next_token(p)
	bin.rhs = parse_expr_atom(p) or_return

	bin_expr := bin
	bin_last := bin
	expr      = bin

	for {
		op, is_bin_op = token_to_binary_op(p.token.kind)
		if !is_bin_op do return

		bin = new(Expr_Binary, p.allocator) or_return

		bin.op       = op
		bin.op_token = p.token

		parser_next_token(p)
		bin.rhs = parse_expr_atom(p) or_return

		if precedence_table[op] <= precedence_table[bin_last.op] {
			if precedence_table[op] <= precedence_table[bin_expr.op] &&
			   op != .Pow /* exponentiation is right-associative */ {
				/*     +
				|     / \
				|    *   c
				|   / \
				|  a   b
				*/
				bin.lhs  = bin_expr
				expr     = bin
				bin_expr = bin
			} else {
				/* (a + b^c * d)   +  <- bin_expr
				|                 / \   
				|                a   *  
				|                   / \ 
				|     bin_last ->  ^   d
				|                 / \   
				|                b   c
				*/   
				bin.lhs      = bin_expr.rhs
				bin_expr.rhs = bin
			}
		} else {
			/* (a + b * c^d)   +  <- bin_expr
			|                 / \      
			|                a   *  <- bin_last    
			|                   / \    
			|                  b   ^   
			|                     / \  
			|                    c   d
			*/                                                   
			bin.lhs      = bin_last.rhs
			bin_last.rhs = bin
		}
		
		bin_last = bin
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
	op: Expr_Unary_Op

	#partial switch p.token.kind {
	case .Add: op = .Pos
	case .Sub: op = .Neg
	case: unreachable()
	}

	unary = new(Expr_Unary, p.allocator) or_return

	unary.op = op
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
	ident.name = token_string(p.src, p.token)
	ident.token = p.token

	parser_next_token(p)

	return
}

@(require_results)
parse_number :: proc (p: ^Parser) -> (number: ^Expr_Number, err: Parse_Error)
{
	assert(p.token.kind == .Num)

	number = new(Expr_Number, p.allocator) or_return
	
	value, ok := strconv.parse_f64(token_string(p.src, p.token))
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
