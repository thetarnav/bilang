package bilang

import "core:fmt"
import "core:mem"
import "core:io"
import "core:os"
import "core:strings"
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



write_number :: proc(w: io.Writer, n: f64, highlight := true)
{
	if highlight do fmt.wprint(w, "\e[0;33m")
	fmt.wprint(w, n)
	if highlight do fmt.wprint(w, "\e[0m")
}


/*

AST

*/


print_decls :: proc (decls: []Decl, highlight := true, fd := os.stdout)
{
	w := _scope_handle_writer(fd)
	write_decls(w^, decls, highlight)
}
write_decls :: proc (w: io.Writer, decls: []Decl, highlight := true)
{
	for decl in decls {
		write_decl(w, decl, highlight)
	}
}

print_decl :: proc (decl: Decl, highlight := true, fd := os.stdout)
{
	w := _scope_handle_writer(fd)
	write_decl(w^, decl, highlight)
}
write_decl :: proc (w: io.Writer, decl: Decl, highlight := true)
{
	write_expr(w, decl.lhs, highlight)
	fmt.wprint(w, " ")
	if highlight do fmt.wprint(w, "\e[0;36m")
	fmt.wprint(w, "=")
	if highlight do fmt.wprint(w, "\e[0m")
	fmt.wprint(w, " ")
	write_expr(w, decl.rhs, highlight)
	fmt.wprint(w, "\n")
}

print_expr :: proc (expr: Expr, highlight := true, fd := os.stdout)
{
	w := _scope_handle_writer(fd)
	write_expr(w^, expr, highlight)
}
write_expr :: proc (w: io.Writer, expr: Expr, highlight := true)
{
	switch v in expr {
	case ^Expr_Binary: write_binary(w, v, highlight)
	case ^Expr_Unary:  write_unary (w, v, highlight)
	case ^Expr_Ident:  write_ident (w, v, highlight)
	case ^Expr_Number: write_expr_number(w, v, highlight)
	}

	return
}

print_binary :: proc (binary: ^Expr_Binary, highlight := true, fd := os.stdout)
{
	w := _scope_handle_writer(fd)
	write_binary(w^, binary, highlight)
}
write_binary :: proc (w: io.Writer, binary: ^Expr_Binary, highlight := true)
{
	if highlight do fmt.wprint(w, "\e[38;5;240m")
	fmt.wprint(w, "(")
	if highlight do fmt.wprint(w, "\e[0m")

	if highlight do fmt.wprint(w, "\e[0;36m")
	switch binary.op {
	case .Add: fmt.wprint(w, "+")
	case .Sub: fmt.wprint(w, "-")
	case .Mul: fmt.wprint(w, "*")
	case .Div: fmt.wprint(w, "/")
	}
	if highlight do fmt.wprint(w, "\e[0m")

	fmt.wprint(w, " ")
	write_expr(w, binary.lhs, highlight)
	fmt.wprint(w, " ")
	write_expr(w, binary.rhs, highlight)
	if highlight do fmt.wprint(w, "\e[38;5;240m")
	fmt.wprint(w, ")")
	if highlight do fmt.wprint(w, "\e[0m")
}

print_unary :: proc (unary: ^Expr_Unary, highlight := true, fd := os.stdout)
{
	w := _scope_handle_writer(fd)
	write_unary(w^, unary, highlight)
}
write_unary :: proc (w: io.Writer, unary: ^Expr_Unary, highlight := true)
{	
	if highlight do fmt.wprint(w, "\e[38;5;240m")
	fmt.wprint(w, "(")
	if highlight do fmt.wprint(w, "\e[0m")
	
	if highlight do fmt.wprint(w, "\e[0;36m")
	switch unary.op {
	case .Neg: fmt.wprint(w, "-")
	case .Pos: fmt.wprint(w, "+")
	}
	if highlight do fmt.wprint(w, "\e[0m")

	fmt.wprint(w, " ")
	write_expr(w, unary.rhs, highlight)
	if highlight do fmt.wprint(w, "\e[38;5;240m")
	fmt.wprint(w, ")")
	if highlight do fmt.wprint(w, "\e[0m")
}

print_ident :: proc (ident: ^Expr_Ident, highlight := true, fd := os.stdout)
{
	w := _scope_handle_writer(fd)
	write_ident(w^, ident, highlight)
}
write_ident :: proc (w: io.Writer, ident: ^Expr_Ident, highlight := true)
{
	fmt.wprint(w, ident.name)
}

print_number :: proc (number: ^Expr_Number, highlight := true, fd := os.stdout)
{
	w := _scope_handle_writer(fd)
	write_expr_number(w^, number, highlight)
}
write_expr_number :: proc (w: io.Writer, number: ^Expr_Number, highlight := true)
{
	write_number(w, number.value, highlight)
}


/*

CONSTRAINTS

*/


print_contraints :: proc (constrs: []Constraint, highlight := true, fd := os.stdout)
{
	w := _scope_handle_writer(fd)
	write_contraints(w^, constrs, highlight)
}
contraints_to_string :: proc (constrs: []Constraint, highlight := true, allocator := context.allocator) -> (s: string, err: mem.Allocator_Error)
{
	b := strings.builder_make_len_cap(0, 1024, allocator) or_return
	w := strings.to_writer(&b)

	write_contraints(w, constrs)

	return strings.to_string(b), nil
}
write_contraints :: proc (w: io.Writer, constrs: []Constraint, highlight := true)
{
	for constr in constrs {
		fmt.wprint(w, constr.var)
		fmt.wprint(w, ": ")

		write_atom(w, constr.lhs^, highlight)

		if highlight do fmt.wprint(w, "\e[0;36m")
		fmt.wprint(w, " = ")
		if highlight do fmt.wprint(w, "\e[0m")

		write_atom(w, constr.rhs^, highlight)

		fmt.wprint(w, "\n")
	}
}

print_atom :: proc (atom: Atom, highlight := true, fd := os.stdout)
{
	w := _scope_handle_writer(fd)
	write_atom(w^, atom, highlight)
}
write_atom :: proc (w: io.Writer, atom: Atom, highlight := true)
{
	switch a in atom {
	case Atom_Num:
		write_fraction(w, a, highlight)
	case Atom_Var:
		if a.f != FRACTION_IDENTITY {
			write_fraction(w, a, highlight)
		}
		
		fmt.wprint(w, a.name)
	case Atom_Binary:
		if highlight do fmt.wprint(w, "\e[38;5;240m")
		fmt.wprint(w, "(")
		if highlight do fmt.wprint(w, "\e[0m")

		if highlight do fmt.wprint(w, "\e[0;36m")
		switch a.op {
		case .Add: fmt.wprint(w, "+ ")
		case .Mul: fmt.wprint(w, "* ")
		case .Div: fmt.wprint(w, "/ ")
		}
		if highlight do fmt.wprint(w, "\e[0m")

		write_atom(w, a.lhs^, highlight)

		fmt.wprint(w, " ")

		write_atom(w, a.rhs^, highlight)

		if highlight do fmt.wprint(w, "\e[38;5;240m")
		fmt.wprint(w, ")")
		if highlight do fmt.wprint(w, "\e[0m")
	}
}

write_fraction :: proc(w: io.Writer, f: Fraction, highlight := true)
{
	num := f.num / f.den
	write_number(w, num, highlight)
	
	// if f.den == 1 {
	// 	write_number(w, f.num, highlight)
	// }
	// else {
	// 	if highlight do fmt.wprint(w, "\e[38;5;240m")
	// 	fmt.wprint(w, "(")
	// 	if highlight do fmt.wprint(w, "\e[0m")

	// 	write_number(w, f.num, highlight)

	// 	if highlight do fmt.wprint(w, "\e[0;36m")
	// 	fmt.wprint(w, "/")
	// 	if highlight do fmt.wprint(w, "\e[0m")

	// 	write_number(w, f.den, highlight)

	// 	if highlight do fmt.wprint(w, "\e[38;5;240m")
	// 	fmt.wprint(w, ")")
	// 	if highlight do fmt.wprint(w, "\e[0m")
	// }
}
