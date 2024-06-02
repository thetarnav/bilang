package bilang

import "core:fmt"
import "core:io"
import "core:os"
import "core:bufio"

@(private, deferred_out=bufio.writer_flush)
_scope_bufio_writer :: #force_inline proc (fd: os.Handle) -> (b_ptr: ^bufio.Writer) {
	buf: [1024]byte
	b: bufio.Writer
	b_ptr = &b

	bufio.writer_init_with_buf(&b, os.stream_from_handle(fd), buf[:])
	
	return
}

print_decls :: proc (decls: []^Assign, fd := os.stdout)
{
	w := bufio.writer_to_writer(_scope_bufio_writer(fd))
	write_decls(w, decls)
}
write_decls :: proc (w: io.Writer, decls: []^Assign)
{
	for assign in decls {
		write_assign(w, assign)
	}
}

print_assign :: proc (assign: ^Assign, fd := os.stdout)
{
	w := bufio.writer_to_writer(_scope_bufio_writer(fd))
	write_assign(w, assign)
}
write_assign :: proc (w: io.Writer, assign: ^Assign)
{
	write_expr(w, assign.lhs)
	fmt.wprint(w, " \e[0;36m=\e[0m ")
	write_expr(w, assign.rhs)
	fmt.wprint(w, "\n")
}

print_expr :: proc (expr: Expr, fd := os.stdout)
{
	w := bufio.writer_to_writer(_scope_bufio_writer(fd))
	write_expr(w, expr)
}
write_expr :: proc (w: io.Writer, expr: Expr)
{
	switch v in expr {
	case ^Binary: write_binary(w, v)
	case ^Unary:  write_unary (w, v)
	case ^Ident:  write_ident (w, v)
	case ^Number: write_number(w, v)
	}

	return
}

print_binary :: proc (binary: ^Binary, fd := os.stdout)
{
	w := bufio.writer_to_writer(_scope_bufio_writer(fd))
	write_binary(w, binary)
}
write_binary :: proc (w: io.Writer, binary: ^Binary)
{
	fmt.wprint(w, "\e[38;5;240m(\e[0m")

	fmt.wprint(w, "\e[0;36m")
	switch binary.op {
	case .Add: fmt.wprint(w, "+")
	case .Sub: fmt.wprint(w, "-")
	case .Mul: fmt.wprint(w, "*")
	case .Div: fmt.wprint(w, "/")
	}
	fmt.wprint(w, "\e[0m")

	fmt.wprint(w, " ")
	write_expr(w, binary.lhs)
	fmt.wprint(w, " ")
	write_expr(w, binary.rhs)
	fmt.wprint(w, "\e[38;5;240m)\e[0m")
}

print_unary :: proc (unary: ^Unary, fd := os.stdout)
{
	w := bufio.writer_to_writer(_scope_bufio_writer(fd))
	write_unary(w, unary)
}
write_unary :: proc (w: io.Writer, unary: ^Unary)
{	
	fmt.wprint(w, "\e[38;5;240m(\e[0m")
	
	fmt.wprint(w, "\e[0;36m")
	switch unary.op {
	case .Neg: fmt.wprint(w, "-")
	case .Pos: fmt.wprint(w, "+")
	}
	fmt.wprint(w, "\e[0m")

	fmt.wprint(w, " ")
	write_expr(w, unary.expr)
	fmt.wprint(w, "\e[38;5;240m)\e[0m")
}

print_ident :: proc (ident: ^Ident, fd := os.stdout)
{
	w := bufio.writer_to_writer(_scope_bufio_writer(fd))
	write_ident(w, ident)
}
write_ident :: proc (w: io.Writer, ident: ^Ident)
{
	fmt.wprint(w, ident.name)
}

print_number :: proc (number: ^Number, fd := os.stdout)
{
	w := bufio.writer_to_writer(_scope_bufio_writer(fd))
	write_number(w, number)
}
write_number :: proc (w: io.Writer, number: ^Number)
{
	fmt.wprint(w, "\e[0;33m")
	fmt.wprint(w, number.value)
	fmt.wprint(w, "\e[0m")
}
