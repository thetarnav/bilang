package bilang

import "core:unicode/utf8"
import "core:strings"
import "core:mem"
import "core:reflect"


Token_Kind :: enum u8 {
	Invalid,
	EOF,
	EOL,           // \n
	// Comment,       // # Comment
	// Punctuators
	Paren_L,       // (
	Paren_R,       // )
	// Operators
	Eq,            // =
	Add,           // +
	Sub,           // -
	Mul,           // *
	Div,           // /
	Pow,           // ^
	Or,            // |
	// Scalars
	Int,           // 123
	Float,         // 123.456, 0.123, 0, 1e2, 1.2e3
	Str,           // "hello world"
	// Identifiers
	Ident,         // abc_69
}

Token :: struct {
	kind: Token_Kind,
	text: string, // view to Tokenizer.src
}

Tokenizer :: struct {
	src       : string,
	pos_read  : int,
	pos_write : int,
	char      : rune,
	char_width: int,
}


tokenizer_init :: proc "contextless" (t: ^Tokenizer, src: string) {
	t.src = src
	if next_char(t) == utf8.RUNE_BOM {
		t.pos_write = t.pos_read
		next_char(t)
	}
}

@(require_results)
tokenizer_make :: #force_inline proc "contextless" (src: string) -> (t: Tokenizer) {
	tokenizer_init(&t, src)
	return
}
make_tokenizer :: tokenizer_make

next_char :: proc "contextless" (t: ^Tokenizer) -> (char: rune, in_file: bool) #optional_ok #no_bounds_check {
	if t.pos_read >= len(t.src) {
		t.char = 0
		t.char_width = 0
		return 0, false
	}

	ch, width := utf8.decode_rune_in_string(t.src[t.pos_read:])
	t.char = ch
	t.pos_read  += width
	t.char_width = width
	return ch, true
}

make_token :: proc "contextless" (t: ^Tokenizer, kind: Token_Kind) -> (token: Token) {
	token.kind  = kind
	token.text  = t.src[t.pos_write:t.pos_read]
	t.pos_write = t.pos_read
	next_char(t)
	return
}
make_token_go_back :: proc "contextless" (t: ^Tokenizer, kind: Token_Kind) -> (token: Token) {
	token.kind  = kind
	token.text  = t.src[t.pos_write:t.pos_read-t.char_width]
	t.pos_write = t.pos_read - t.char_width
	return
}

@(require_results)
next_token :: proc "contextless" (t: ^Tokenizer) -> (token: Token, in_file: bool) #optional_ok #no_bounds_check {

	if t.pos_read >= len(t.src) && t.char == 0 {
		return make_token(t, .EOF), false
	}
	in_file = true

	switch t.char {
	// Whitespace and comma
	case '\n':
		t.pos_write = t.pos_read - 1
		return make_token(t, .EOL), true
	case ',', ' ', '\t', '\r':
		t.pos_write = t.pos_read
		next_char(t)
		return next_token(t)
	// Ignore Comment
	case '#':
		for {
			switch next_char(t) {
			case 0, '\n', '\r':
				return next_token(t)
			}
		}
	// Punctuators
	case '(': return make_token(t, .Paren_L), true
	case ')': return make_token(t, .Paren_R), true
	// Operators
	case '=': return make_token(t, .Eq), true
	case '+': return make_token(t, .Add), true
	case '-': return make_token(t, .Sub), true
	case '*': return make_token(t, .Mul), true
	case '/': return make_token(t, .Div), true
	case '^': return make_token(t, .Pow), true
	case '|': return make_token(t, .Or), true
	// Str
	case '"':
		valid := true
		for {
			switch next_char(t) {
			case 0:
				return make_token_go_back(t, .Invalid), true // unterminated string
			case '"':
				if valid {
					return make_token(t, .Str), true
				} else {
					return make_token(t, .Invalid), true
				}
			case '\\':
				// handle escape sequences
				switch next_char(t) {
				case 0:
					return make_token_go_back(t, .Invalid), true // unterminated string
				case '"', '\\', 'n', 'r', 't':
					// valid escape sequences
				case:
					valid = false // mark as invalid but continue parsing
				}
			}
		}
	// Number
	// 123
	// 123.456
	case '0'..='9':
		for {
			switch next_char(t) {
			case '0'..='9':
				// continue
			case '.':
				// fraction (123.456)
				switch next_char(t) {
				case '0'..='9':
					// continue
				case:
					return make_token_go_back(t, .Invalid), true
				}
				for {
					switch next_char(t) {
					case '0'..='9':
						// continue
					case 'e', 'E':
						// exponent (123.456e+10)
						switch next_char(t) {
						case '+', '-':
							switch next_char(t) {
							case '0'..='9':
								// continue
							case:
								return make_token_go_back(t, .Invalid), true // invalid like 1e+ or 1e-
							}
						case '0'..='9':
							// continue
						case:
							return make_token_go_back(t, .Invalid), true // invalid like 1e or 1.2e
						}
						for {
							switch next_char(t) {
							case '0'..='9':
								// continue
							case 'a'..='z', 'A'..='Z', '_':
								return make_token_go_back(t, .Invalid), true
							case:
								return make_token_go_back(t, .Float), true // float with exponent
							}
						}
					case 'a'..='z', 'A'..='Z', '_':
						return make_token_go_back(t, .Invalid), true
					case:
						return make_token_go_back(t, .Float), true // float
					}
				}
			case 'e', 'E':
				// exponent (123e+10)
				switch next_char(t) {
				case '+', '-':
					switch next_char(t) {
					case '0'..='9':
						// continue
					case:
						return make_token_go_back(t, .Invalid), true // invalid like 1e+ or 1e-
					}
				case '0'..='9':
					// continue
				case:
					return make_token_go_back(t, .Invalid), true // invalid like 1e
				}
				for {
					switch next_char(t) {
					case '0'..='9':
						// continue
					case 'a'..='z', 'A'..='Z', '_':
						return make_token_go_back(t, .Invalid), true
					case:
						return make_token_go_back(t, .Float), true // int with exponent, treated as float
					}
				}
			case 'a'..='z', 'A'..='Z', '_':
				return make_token_go_back(t, .Invalid), true
			case:
				return make_token_go_back(t, .Int), true // int
			}
		}
	// Identifiers
	case 'a'..='z', 'A'..='Z', '_':
		for {
			switch next_char(t) {
			case 'a'..='z', 'A'..='Z', '0'..='9', '_': continue
			}
			break
		}
		return make_token_go_back(t, .Ident), true
	case:
		return make_token(t, .Invalid), true
	}
}
tokenizer_next :: next_token

@(require_results)
next_token_expect :: proc(
	t: ^Tokenizer,
	expected: Token_Kind,
) -> (token: Token, ok: bool) {
	token = next_token(t)
	ok    = token.kind == expected
	return
}

token_pos :: proc (src: string, token: Token) -> int
{
	src_ptr := uintptr(raw_data(src))
	tok_ptr := uintptr(raw_data(token.text))
	tok_pos := int(tok_ptr-src_ptr)
	assert(0 <= tok_pos && tok_pos < len(src))
	return tok_pos
}

/*
The returned line and column are 0-based.
*/
@(require_results)
token_line_col :: proc (src: string, token: Token) -> (line, col: int)
{
	for ch in src[:token_pos(src, token)] {
		if ch == '\n' {
			line += 1
			col   = 0
		} else {
			col += 1
		}
	}
	return
}

@(require_results)
display_token_in_line :: proc (
	src: string, token: Token,
	allocator := context.allocator,
) -> (text: string, err: mem.Allocator_Error) #optional_allocator_error
{
	b := strings.builder_make(0, 128, allocator) or_return
	defer shrink(&b.buf)

	pos       := token_pos(src, token)
	line, col := token_line_col(src, token)

	start := pos - col
	end   := pos + len(token.text)

	// extend end to eol
	for i := 0; i < 40 && end < len(src); i += 1 {
		if src[end] == '\n' do break
		end += 1
	}

	/*
	.Invalid "????" at 1:10
	abc = 123 ???? 456
	          ^^^^
	*/

	strings.write_string(&b, ".")
	strings.write_string(&b, reflect.enum_string(token.kind))
	strings.write_string(&b, " \"")
	strings.write_string(&b, token.text)
	strings.write_string(&b, "\" at ")
	strings.write_int   (&b, line+1)
	strings.write_string(&b, ":")
	strings.write_int   (&b, col+1)
	strings.write_string(&b, "\n")
	strings.write_string(&b, src[start:end])
	strings.write_rune  (&b, '\n')
	for _ in 0..<col {
		strings.write_rune(&b, ' ')
	}
	for _ in token.text {
		strings.write_rune(&b, '^')
	}
	strings.write_rune(&b, '\n')

	text = strings.to_string(b)

	return
}
