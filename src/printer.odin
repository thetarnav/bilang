package bilang

import "core:mem"
import "core:io"
import "core:os"
import "core:strings"
import "core:strconv"
import "core:bufio"
import "core:terminal/ansi"


Printer_Writer :: struct {
	buf: [1024]byte,
	b:   bufio.Writer,
	w:   io.Writer,
}

@(private, deferred_in=_scope_handle_writer_flush)
_scope_handle_writer :: proc (w: ^Printer_Writer, fd: os.Handle)
{
	bufio.writer_init_with_buf(&w.b, os.stream_from_handle(fd), w.buf[:])
	w.w = bufio.writer_to_writer(&w.b)
}
@(private)
_scope_handle_writer_flush :: proc (w: ^Printer_Writer, _: os.Handle)
{
	bufio.writer_flush(&w.b)
}


Writer_Options :: struct {
	highlight: bool,
	parens: bool,
}

write_string :: io.write_string
write_quoted_string :: io.write_quoted_string
write_space :: proc (w: io.Writer) {
	io.write_string(w, " ")
}
write_newline :: proc (w: io.Writer) {
	io.write_string(w, "\n")
}

@(private)
_write_f64 :: proc(w: io.Writer, val: f64, n_written: ^int = nil) -> (n: int, err: io.Error) {
	buf: [386]byte

	str := generic_ftoa(buf[1:], val, 8*size_of(val))
	s := buf[:len(str)+1]
	
	if s[1] == '+' || s[1] == '-' {
		s = s[1:]
	} else {
		s[0] = '+'
	}

	if s[0] == '+' {
		s = s[1:]
	}

	return io.write_string(w, string(s), n_written)
}
@private
_write_int :: proc(w: io.Writer, val: i64, n_written: ^int = nil) -> (n: int, err: io.Error) {
	buf: [32]byte
	str := strconv.write_int(buf[:], val, 10)
	return io.write_string(w, string(str), n_written)
}

Highlight_Kind :: enum {Reset, Num, Str, Punct, Op}

write_highlight :: proc (w: io.Writer, kind: Highlight_Kind, opts: Writer_Options = {})
{
	using ansi
	if opts.highlight do switch kind {
	case .Reset: write_string(w, CSI+RESET+SGR)
	case .Num:   write_string(w, CSI+FG_YELLOW+SGR)
	case .Str:   write_string(w, CSI+FG_GREEN+SGR)
	case .Op:    write_string(w, CSI+FG_CYAN+SGR)
	case .Punct: write_string(w, CSI+FG_COLOR_8_BIT+";240"+SGR)
	}
}

write_float :: proc (w: io.Writer, n: f64, opts: Writer_Options = {})
{
	write_highlight(w, .Num, opts)
	_write_f64(w, n)
	write_highlight(w, .Reset, opts)
}
write_int :: proc (w: io.Writer, n: int, opts: Writer_Options = {})
{
	write_highlight(w, .Num, opts)
	_write_int(w, i64(n))
	write_highlight(w, .Reset, opts)
}

write_punct :: proc (w: io.Writer, c: string, opts: Writer_Options = {})
{
	write_highlight(w, .Punct, opts)
	write_string(w, c)
	write_highlight(w, .Reset, opts)
}

write_operator :: proc (w: io.Writer, c: string, opts: Writer_Options = {})
{
	write_highlight(w, .Op, opts)
	write_string(w, c)
	write_highlight(w, .Reset, opts)
}

write_punct_token :: proc (w: io.Writer, t: Token_Kind, opts: Writer_Options = {})
{
	#partial switch t {
	case .Paren_L: write_punct(w, "(", opts)
	case .Paren_R: write_punct(w, ")", opts)
	}
}

write_operator_token :: proc (w: io.Writer, t: Token_Kind, opts: Writer_Options = {})
{
	#partial switch t {
	case .Eq:  write_operator(w, "=", opts)
	case .Add: write_operator(w, "+", opts)
	case .Sub: write_operator(w, "-", opts)
	case .Mul: write_operator(w, "*", opts)
	case .Div: write_operator(w, "/", opts)
	case .Pow: write_operator(w, "^", opts)
	case .Or:  write_operator(w, "|", opts)
	}
}

write_paren :: proc (w: io.Writer, t: Token_Kind, opts: Writer_Options = {})
{
	assert(t == .Paren_L || t == .Paren_R)
	if opts.parens {
		write_punct_token(w, t, opts)
	}
}


/*

AST

*/


print_exprs :: proc (exprs: []Expr, opts: Writer_Options = {}, fd := os.stdout) {
	w: Printer_Writer
	_scope_handle_writer(&w, fd)
	write_exprs(w.w, exprs, opts)
}
write_exprs :: proc (w: io.Writer, exprs: []Expr, opts: Writer_Options = {}) {
	for expr in exprs {
		write_expr(w, expr, opts)
		write_newline(w)
	}
}

print_expr :: proc (expr: Expr, opts: Writer_Options = {}, fd := os.stdout)
{
	w: Printer_Writer
	_scope_handle_writer(&w, fd)
	write_expr(w.w, expr, opts)
}
write_expr :: proc (w: io.Writer, expr: Expr, opts: Writer_Options = {})
{
	switch v in expr {
	case ^Expr_Binary: write_binary(w, v, opts)
	case ^Expr_Unary:  write_unary (w, v, opts)
	case ^Expr_Single: write_single(w, v, opts)
	}
	return
}

print_binary :: proc (binary: ^Expr_Binary, opts: Writer_Options = {}, fd := os.stdout)
{
	w: Printer_Writer
	_scope_handle_writer(&w, fd)
	write_binary(w.w, binary, opts)
}
write_binary :: proc (w: io.Writer, binary: ^Expr_Binary, opts: Writer_Options = {})
{
	write_paren(w, .Paren_L, opts)

	{
		opts := opts
		opts.parens = true
		write_expr(w, binary.lhs, opts)
	}
	write_space(w)
	write_operator(w, binary.op_token.text, opts)
	write_space(w)
	{
		opts := opts
		opts.parens = true
		write_expr(w, binary.rhs, opts)
	}

	write_paren(w, .Paren_R, opts)
}

print_unary :: proc (unary: ^Expr_Unary, opts: Writer_Options = {}, fd := os.stdout)
{
	w: Printer_Writer
	_scope_handle_writer(&w, fd)
	write_unary(w.w, unary, opts)
}
write_unary :: proc (w: io.Writer, unary: ^Expr_Unary, opts: Writer_Options = {})
{	
	write_paren(w, .Paren_L, opts)
	write_operator(w, unary.op_token.text, opts)
	write_space(w)
	{
		opts := opts
		opts.parens = true
		write_expr(w, unary.rhs, opts)
	}
	write_paren(w, .Paren_R, opts)
}

print_single :: proc (single: ^Expr_Single, opts: Writer_Options = {}, fd := os.stdout)
{
	w: Printer_Writer
	_scope_handle_writer(&w, fd)
	write_single(w.w, single, opts)
}
write_single :: proc (w: io.Writer, single: ^Expr_Single, opts: Writer_Options = {})
{
	#partial switch single.token.kind {
	case .Float, .Int:
		write_highlight(w, .Num, opts)
		write_string(w, single.token.text)
		write_highlight(w, .Reset, opts)
	case .Str:
		write_highlight(w, .Str, opts)
		write_string(w, single.token.text)
		write_highlight(w, .Reset, opts)
	case .Ident:
		write_string(w, single.token.text)
	case:
		unreachable()
	}
}


/*

CONSTRAINTS

*/


print_constraints :: proc (constrs: []Constraint, opts: Writer_Options = {}, fd := os.stdout) {
	w: Printer_Writer
	_scope_handle_writer(&w, fd)
	write_constraints(w.w, constrs, opts)
}

@require_results
constraints_to_string :: proc (
	constrs: []Constraint,
	opts   : Writer_Options = {},
	allocator := context.allocator,
) -> (s: string, err: mem.Allocator_Error) #optional_allocator_error
{
	b := strings.builder_make_len_cap(0, 1024, allocator) or_return
	w := strings.to_writer(&b)

	write_constraints(w, constrs, opts)

	return strings.to_string(b), nil
}
write_constraints :: proc (w: io.Writer, constrs: []Constraint, opts: Writer_Options = {})
{
	for constr in constrs {
		write_string(w, constr.var)
		write_punct(w, ": ", opts)
		write_atom(w, constr.atom^, opts)
		write_newline(w)
	}
}

print_atom :: proc (atom: Atom, opts: Writer_Options = {}, fd := os.stdout)
{
	w: Printer_Writer
	_scope_handle_writer(&w, fd)
	write_atom(w.w, atom, opts)
}
write_atom :: proc (w: io.Writer, atom: Atom, opts: Writer_Options = {})
{
	switch atom.kind {
	case .Int:
		write_int(w, atom.int, opts)
	case .Float:
		write_float(w, atom.float, opts)
	case .Str:
		write_highlight(w, .Str, opts)
		write_quoted_string(w, atom.str)
		write_highlight(w, .Reset, opts)
	case .Var:
		write_string(w, atom.var)
	case .Add, .Mul, .Div, .Pow, .Or, .Eq:

		// x*-1  ->  -x
		display_neg: if atom.kind == .Mul {
			val: ^Atom
			if atom_val_equals(atom.lhs^, -1) {
				val = atom.rhs
			} else if atom_val_equals(atom.rhs^, -1) {
				val = atom.lhs
			} else {
				break display_neg
			}

			write_operator_token(w, .Sub, opts)
			write_atom(w, val^, opts)
			return
		}

		op: Token_Kind
		#partial switch atom.kind {
		case .Add: op = .Add
		case .Mul: op = .Mul 
		case .Div: op = .Div
		case .Pow: op = .Pow
		case .Or:  op = .Or
		case .Eq:  op = .Eq
		}

		is_deep := (atom.lhs.kind != atom.kind && atom_is_binary(atom.lhs^)) ||
		           (atom.rhs.kind != atom.kind && atom_is_binary(atom.rhs^))
		
		child_opts := opts
		child_opts.parens = atom.kind != .Eq &&
		                    (atom.lhs.kind != atom.kind || (atom.kind != .Mul && atom.kind != .Add))

		if is_deep do write_paren(w, .Paren_L, opts)
		write_atom(w, atom.lhs^, child_opts)
		if is_deep || atom.kind == .Eq do write_space(w)
		write_operator_token(w, op, opts)
		if is_deep || atom.kind == .Eq do write_space(w)
		write_atom(w, atom.rhs^, child_opts)
		if is_deep do write_paren(w, .Paren_R, opts)
	}
}
