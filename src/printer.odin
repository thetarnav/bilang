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

write_number :: proc(w: io.Writer, n: f64, highlight := false)
{
	if highlight do io.write_string(w, "\e[0;33m")
	_write_f64(w, n)
	if highlight do io.write_string(w, "\e[0m")
}

write_punct :: proc(w: io.Writer, c: string, highlight := false)
{
	if highlight do io.write_string(w, "\e[38;5;240m")
	io.write_string(w, c)
	if highlight do io.write_string(w, "\e[0m")
}

write_operator :: proc(w: io.Writer, c: string, highlight := false)
{
	if highlight do io.write_string(w, "\e[0;36m")
	io.write_string(w, c)
	if highlight do io.write_string(w, "\e[0m")
}


/*

AST

*/


print_decls :: proc (decls: []Decl, highlight := false, fd := os.stdout)
{
	w := _scope_handle_writer(fd)
	write_decls(w^, decls, highlight)
}
write_decls :: proc (w: io.Writer, decls: []Decl, highlight := false)
{
	for decl in decls {
		write_decl(w, decl, highlight)
	}
}

print_decl :: proc (decl: Decl, highlight := false, fd := os.stdout)
{
	w := _scope_handle_writer(fd)
	write_decl(w^, decl, highlight)
}
write_decl :: proc (w: io.Writer, decl: Decl, highlight := false)
{
	write_expr(w, decl.lhs, highlight)
	write_operator(w, " = ", highlight)
	write_expr(w, decl.rhs, highlight)
	io.write_string(w, "\n")
}

print_expr :: proc (expr: Expr, highlight := false, fd := os.stdout)
{
	w := _scope_handle_writer(fd)
	write_expr(w^, expr, highlight)
}
write_expr :: proc (w: io.Writer, expr: Expr, highlight := false)
{
	switch v in expr {
	case ^Expr_Binary: write_binary(w, v, highlight)
	case ^Expr_Unary:  write_unary (w, v, highlight)
	case ^Expr_Ident:  write_ident (w, v, highlight)
	case ^Expr_Number: write_expr_number(w, v, highlight)
	}

	return
}

print_binary :: proc (binary: ^Expr_Binary, highlight := false, fd := os.stdout)
{
	w := _scope_handle_writer(fd)
	write_binary(w^, binary, highlight)
}
write_binary :: proc (w: io.Writer, binary: ^Expr_Binary, highlight := false)
{
	write_punct(w, "(", highlight)

	switch binary.op {
	case .Add: write_operator(w, "+", highlight)
	case .Sub: write_operator(w, "-", highlight)
	case .Mul: write_operator(w, "*", highlight)
	case .Div: write_operator(w, "/", highlight)
	}

	io.write_string(w, " ")
	write_expr(w, binary.lhs, highlight)
	io.write_string(w, " ")
	write_expr(w, binary.rhs, highlight)

	write_punct(w, ")", highlight)
}

print_unary :: proc (unary: ^Expr_Unary, highlight := false, fd := os.stdout)
{
	w := _scope_handle_writer(fd)
	write_unary(w^, unary, highlight)
}
write_unary :: proc (w: io.Writer, unary: ^Expr_Unary, highlight := false)
{	
	write_punct(w, "(", highlight)
	
	switch unary.op {
	case .Neg: write_operator(w, "-", highlight)
	case .Pos: write_operator(w, "+", highlight)
	}

	io.write_string(w, " ")
	write_expr(w, unary.rhs, highlight)
	
	write_punct(w, ")", highlight)
}

print_ident :: proc (ident: ^Expr_Ident, highlight := false, fd := os.stdout)
{
	w := _scope_handle_writer(fd)
	write_ident(w^, ident, highlight)
}
write_ident :: proc (w: io.Writer, ident: ^Expr_Ident, highlight := false)
{
	io.write_string(w, ident.name)
}

print_number :: proc (number: ^Expr_Number, highlight := false, fd := os.stdout)
{
	w := _scope_handle_writer(fd)
	write_expr_number(w^, number, highlight)
}
write_expr_number :: proc (w: io.Writer, number: ^Expr_Number, highlight := false)
{
	write_number(w, number.value, highlight)
}


/*

CONSTRAINTS

*/


print_contraints :: proc (constrs: []Constraint, params := false, highlight := false, fd := os.stdout)
{
	w := _scope_handle_writer(fd)
	write_contraints(w^, constrs, params=params, highlight=highlight)
}
@(require_results)
contraints_to_string :: proc (
	constrs   : []Constraint,
	params    := false,
	highlight := false,
	allocator := context.allocator,
) -> (s: string, err: mem.Allocator_Error)
{
	b := strings.builder_make_len_cap(0, 1024, allocator) or_return
	w := strings.to_writer(&b)

	write_contraints(w, constrs, params=params, highlight=highlight)

	return strings.to_string(b), nil
}
write_contraints :: proc (w: io.Writer, constrs: []Constraint, params := false, highlight := false)
{
	for constr in constrs {
		io.write_string(w, constr.var)
		write_punct(w, ": ", highlight=highlight)

		write_atom(w, constr.lhs^, params=params, highlight=highlight)
		write_operator(w, " = ", highlight=highlight)
		write_atom(w, constr.rhs^, params=params, highlight=highlight)

		io.write_string(w, "\n")
	}
}

print_atom :: proc (atom: Atom, params := true, highlight := false, fd := os.stdout)
{
	w := _scope_handle_writer(fd)
	write_atom(w^, atom, params=params, highlight=highlight)
}
write_atom :: proc (w: io.Writer, atom: Atom, params := true, highlight := false)
{
	switch a in atom {
	case Atom_Num:
		write_fraction(w, a, highlight=highlight)
	case Atom_Var:
		switch a.f.num / a.f.den {
		case 1:
			// do nothing
		case -1:
			write_operator(w, "-", highlight=highlight)
		case:
			write_fraction(w, a.f, highlight=highlight)
		}
		
		io.write_string(w, a.name)
	case Atom_Add:
		write_atom_operation(w, " + ", a.addends[:], params=params, highlight=highlight)
	case Atom_Mul:
		write_atom_operation(w, " * ", a.factors[:], params=params, highlight=highlight)
	case Atom_Div:
		write_atom_operation(w, " / ", {a.top^, a.bot^}, params=params, highlight=highlight)
	}
}

write_atom_operation :: proc(w: io.Writer, op: string, atoms: []Atom, params := true, highlight := false)
{
	if params {
		write_punct(w, "(", highlight=highlight)
	}

	for atom, i in atoms {
		write_atom(w, atom, params=true, highlight=highlight)
		if i < len(atoms)-1 {
			write_operator(w, op, highlight=highlight)
		}
	}

	if params {
		write_punct(w, ")", highlight=highlight)
	}
}

write_fraction :: proc(w: io.Writer, f: Fraction, highlight := false)
{
	num := f.num / f.den
	write_number(w, num, highlight=highlight)
	
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
