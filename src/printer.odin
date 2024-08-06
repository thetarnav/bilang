package bilang

import "core:mem"
import "core:io"
import "core:os"
import "core:strings"
import "core:strconv"
import "core:bufio"


@(private, deferred_out=_scope_handle_writer_flush)
_scope_handle_writer :: #force_inline proc (fd: os.Handle) -> (w_ptr: ^io.Writer)
{
	buf: [1024]byte
	b: bufio.Writer
	bufio.writer_init_with_buf(&b, os.stream_from_handle(fd), buf[:])

	w := bufio.writer_to_writer(&b)
	w_ptr = &w // sidestep returning a pointer to a local variable warning (the proc is inlined)
	
	return
}
@(private)
_scope_handle_writer_flush :: proc (w: ^io.Writer)
{
	bufio.writer_flush(cast(^bufio.Writer)w.data) // writer_to_writer decls w.data to the bufio.Writer
}


Writer_Options :: struct {
	highlight: bool,
	parens: bool,
}


@(private)
_write_f64 :: proc(w: io.Writer, val: f64, n_written: ^int = nil) -> (n: int, err: io.Error) {
	buf: [386]byte

	str := strconv.append_float(buf[1:], val, 'g', 2*size_of(val), 8*size_of(val))
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

write_number :: proc(w: io.Writer, n: f64, opts: Writer_Options = {})
{
	if opts.highlight do io.write_string(w, "\e[0;33m")
	_write_f64(w, n)
	if opts.highlight do io.write_string(w, "\e[0m")
}

write_punct :: proc(w: io.Writer, c: string, opts: Writer_Options = {})
{
	if opts.highlight do io.write_string(w, "\e[38;5;240m")
	io.write_string(w, c)
	if opts.highlight do io.write_string(w, "\e[0m")
}

write_operator :: proc(w: io.Writer, c: string, opts: Writer_Options = {})
{
	if opts.highlight do io.write_string(w, "\e[0;36m")
	io.write_string(w, c)
	if opts.highlight do io.write_string(w, "\e[0m")
}


/*

AST

*/


print_decls :: proc (decls: []Decl, opts: Writer_Options = {}, fd := os.stdout) {
	w := _scope_handle_writer(fd)
	write_decls(w^, decls, opts)
}
write_decls :: proc (w: io.Writer, decls: []Decl, opts: Writer_Options = {}) {
	for decl in decls {
		write_decl(w, decl, opts)
	}
}

print_decl :: proc (decl: Decl, opts: Writer_Options = {}, fd := os.stdout) {
	w := _scope_handle_writer(fd)
	write_decl(w^, decl, opts)
}
write_decl :: proc (w: io.Writer, decl: Decl, opts: Writer_Options = {}) {
	write_expr(w, decl.lhs, opts)
	write_operator(w, " = ", opts)
	write_expr(w, decl.rhs, opts)
	io.write_string(w, "\n")
}

print_expr :: proc (expr: Expr, opts: Writer_Options = {}, fd := os.stdout)
{
	w := _scope_handle_writer(fd)
	write_expr(w^, expr, opts)
}
write_expr :: proc (w: io.Writer, expr: Expr, opts: Writer_Options = {})
{
	switch v in expr {
	case ^Expr_Binary: write_binary(w, v, opts)
	case ^Expr_Unary:  write_unary (w, v, opts)
	case ^Expr_Ident:  write_ident (w, v, opts)
	case ^Expr_Number: write_expr_number(w, v, opts)
	}
	return
}

print_binary :: proc (binary: ^Expr_Binary, opts: Writer_Options = {}, fd := os.stdout)
{
	w := _scope_handle_writer(fd)
	write_binary(w^, binary, opts)
}
write_binary :: proc (w: io.Writer, binary: ^Expr_Binary, opts: Writer_Options = {})
{
	if opts.parens do write_punct(w, "(", opts)

	{
		opts := opts
		opts.parens = true
		write_expr(w, binary.lhs, opts)
	}
	io.write_string(w, " ")

	switch binary.op {
	case .Add: write_operator(w, "+", opts)
	case .Sub: write_operator(w, "-", opts)
	case .Mul: write_operator(w, "*", opts)
	case .Div: write_operator(w, "/", opts)
	case .Pow: write_operator(w, "^", opts)
	}

	io.write_string(w, " ")
	{
		opts := opts
		opts.parens = true
		write_expr(w, binary.rhs, opts)
	}

	if opts.parens do write_punct(w, ")", opts)
}

print_unary :: proc (unary: ^Expr_Unary, opts: Writer_Options = {}, fd := os.stdout)
{
	w := _scope_handle_writer(fd)
	write_unary(w^, unary, opts)
}
write_unary :: proc (w: io.Writer, unary: ^Expr_Unary, opts: Writer_Options = {})
{	
	if opts.parens do write_punct(w, "(", opts)
	
	switch unary.op {
	case .Neg: write_operator(w, "-", opts)
	case .Pos: write_operator(w, "+", opts)
	}

	io.write_string(w, " ")
	{
		opts := opts
		opts.parens = true
		write_expr(w, unary.rhs, opts)
	}

	if opts.parens do write_punct(w, ")", opts)
}

print_ident :: proc (ident: ^Expr_Ident, opts: Writer_Options = {}, fd := os.stdout)
{
	w := _scope_handle_writer(fd)
	write_ident(w^, ident, opts)
}
write_ident :: proc (w: io.Writer, ident: ^Expr_Ident, opts: Writer_Options = {})
{
	io.write_string(w, ident.name)
}

print_number :: proc (number: ^Expr_Number, opts: Writer_Options = {}, fd := os.stdout)
{
	w := _scope_handle_writer(fd)
	write_expr_number(w^, number, opts)
}
write_expr_number :: proc (w: io.Writer, number: ^Expr_Number, opts: Writer_Options = {})
{
	write_number(w, number.value, opts)
}


/*

CONSTRAINTS

*/


print_contraints :: proc (constrs: []Constraint, opts: Writer_Options = {}, fd := os.stdout) {
	w := _scope_handle_writer(fd)
	write_contraints(w^, constrs, opts)
}

@require_results
contraints_to_string :: proc (
	constrs: []Constraint,
	opts   : Writer_Options = {},
	allocator := context.allocator,
) -> (s: string, err: mem.Allocator_Error) #optional_allocator_error
{
	b := strings.builder_make_len_cap(0, 1024, allocator) or_return
	w := strings.to_writer(&b)

	write_contraints(w, constrs, opts)

	return strings.to_string(b), nil
}
write_contraints :: proc (w: io.Writer, constrs: []Constraint, opts: Writer_Options = {})
{
	for constr in constrs {
		io.write_string(w, constr.var)
		write_punct(w, ": ", opts)

		write_atom(w, constr.lhs^, opts)
		write_operator(w, " = ", opts)
		write_atom(w, constr.rhs^, opts)

		io.write_string(w, "\n")
	}
}

print_atom :: proc (atom: Atom, opts: Writer_Options = {}, fd := os.stdout)
{
	w := _scope_handle_writer(fd)
	write_atom(w^, atom, opts)
}
write_atom :: proc (w: io.Writer, atom: Atom, opts: Writer_Options = {})
{
	switch a in atom {
	case Atom_Num:
		write_fraction(w, a, opts)
	case Atom_Var:
		switch a.f.num / a.f.den {
		case 1:
			// do nothing
		case -1:
			write_operator(w, "-", opts)
		case:
			write_fraction(w, a.f, opts)
			write_operator(w, "*", opts)
		}
		
		io.write_string(w, a.name)
	case Atom_Add, Atom_Mul, Atom_Div, Atom_Pow:

		op: string
		atoms: []Atom
		opts := opts

		#partial switch v in a {
		case Atom_Add:
			op = " + "
			atoms = v.addends[:]
		case Atom_Mul:
			op = " * "
			atoms = v.factors[:]
		case Atom_Div:
			op = " / "
			atoms = {v.lhs^, v.rhs^}
		case Atom_Pow:
			op = "^"
			opts.parens = false
			atoms = {v.lhs^, v.rhs^}
		}

		if opts.parens {
			write_punct(w, "(", opts)
		}


		for item, i in atoms {
			opts := opts
			opts.parens = true
			write_atom(w, item, opts)
			if i < len(atoms)-1 {
				write_operator(w, op, opts)
			}
		}

		if opts.parens {
			write_punct(w, ")", opts)
		}
	}
}

write_fraction :: proc(w: io.Writer, f: Fraction, opts: Writer_Options = {})
{
	num := f.num / f.den
	write_number(w, num, opts)
	
	// if f.den == 1 {
	// 	write_number(w, f.num, highlight)
	// }
	// else {
	// 	if highlight do io.write_string(w, "\e[38;5;240m")
	// 	io.write_string(w, "(")
	// 	if highlight do io.write_string(w, "\e[0m")

	// 	write_number(w, f.num, highlight)

	// 	if highlight do io.write_string(w, "\e[0;36m")
	// 	io.write_string(w, "/")
	// 	if highlight do io.write_string(w, "\e[0m")

	// 	write_number(w, f.den, highlight)

	// 	if highlight do io.write_string(w, "\e[38;5;240m")
	// 	io.write_string(w, ")")
	// 	if highlight do io.write_string(w, "\e[0m")
	// }
}
