package bilang

import "core:unicode/utf8"
import "core:strings"
import "core:mem"


Token_Kind :: enum u8 {
	Invalid,
	EOF,
	Comment,       // # Comment
	// Punctuators
	Paren_L,       // (
	Paren_R,       // )
	// Operators
	Eq,            // =
	Add,           // +
	Sub,           // -
	Mul,           // *
	Div,           // /
	// Scalars
	Num,           // 123, 123.456, 0.123, 0
	// Identifiers
	Ident,         // abc_69
}

Token :: struct {
	kind: Token_Kind,
	pos : int, // offset to Tokenizer.src, 0-based
	len : int,
	line: int, // 1-based
	col : int, // 1-based
}

Tokenizer :: struct {
	src       : string,
	pos_read  : int,
	pos_write : int,
	line      : int,
	line_pos  : int,
	char      : rune,
	char_width: int,
}


tokenizer_init :: proc "contextless" (t: ^Tokenizer, src: string) {
	t.src = src
	t.line = 1
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

token_string :: #force_inline proc "contextless" (src: string, t: Token) -> (text: string) {
	return src[t.pos:t.pos + t.len]
}

next_char :: proc "contextless" (t: ^Tokenizer) -> (char: rune, in_file: bool) #optional_ok #no_bounds_check {
	if t.pos_read >= len(t.src) {
		t.char = 0
		t.pos_read = len(t.src)+1
		t.char_width = 1
		return 0, false
	}

	ch, width := utf8.decode_rune_in_string(t.src[t.pos_read:])
	t.char = ch
	t.pos_read += width
	t.char_width = width
	return ch, true
}

make_token :: proc "contextless" (t: ^Tokenizer, kind: Token_Kind) -> (token: Token) {
	token.kind  = kind
	token.pos   = t.pos_write
	token.len   = t.pos_read - t.pos_write
	token.line  = t.line
	token.col   = t.pos_write - t.line_pos
	t.pos_write = t.pos_read
	next_char(t)
	return
}
make_token_go_back :: proc "contextless" (t: ^Tokenizer, kind: Token_Kind) -> (token: Token) {
	token.kind  = kind
	token.pos   = t.pos_write
	token.len   = t.pos_read - t.pos_write - t.char_width
	token.line  = t.line
	token.col   = t.pos_write - t.line_pos
	t.pos_write = t.pos_read - t.char_width
	return
}

@(require_results)
next_token :: proc "contextless" (t: ^Tokenizer) -> (token: Token, in_file: bool) #optional_ok #no_bounds_check {

	if t.pos_read == len(t.src) {
		return make_token(t, .EOF), true
	}
	if t.pos_read > len(t.src) {
		return make_token(t, .EOF), false
	}
	in_file = true

	switch t.char {
	// Whitespace and comma
	case '\n':
		t.line += 1
		t.line_pos = t.pos_read
		fallthrough
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
					case 'a'..='z', 'A'..='Z', '_':
						return make_token_go_back(t, .Invalid), true
					case:
						return make_token_go_back(t, .Num), true // float
					}
				}
			case 'a'..='z', 'A'..='Z', '_':
				return make_token_go_back(t, .Invalid), true
			case:
				return make_token_go_back(t, .Num), true // int
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
next_token_ignore_comments :: proc(t: ^Tokenizer) -> (token: Token) {
	for {
		token = tokenizer_next(t)
		if token.kind != .Comment do return token
	}
}

@(require_results)
next_token_expect :: proc(
	t: ^Tokenizer,
	expected: Token_Kind,
) -> (token: Token, ok: bool) {
	for {
		token = tokenizer_next(t)
		#partial switch token.kind {
		case .Comment:
			continue
		case expected:
			return token, true
		case:
			return token, false
		}
	}
}

@(require_results)
display_token_in_line :: proc (
	src:   string,
	token: Token,
	allocator := context.allocator,
) -> (text: string, err: mem.Allocator_Error) #optional_allocator_error {

	b := strings.builder_make_len_cap(0, 128, allocator) or_return

	start := token.pos - token.col
	end   := token.pos + token.len

	// extend end to eol
	for i := 0; i < 40 && end < len(src); i += 1 {
		if src[end] == '\n' do break
		end += 1
	}

	/*
	1:10 abc = 123 ???? 456
	               ^^^^
	*/	

	strings.write_int   (&b, token.line)
	strings.write_string(&b, ":")
	strings.write_int   (&b, token.col)
	strings.write_string(&b, " ")
	strings.write_string(&b, src[start:end])
	strings.write_rune  (&b, '\n')
	for _ in 0..<token.col {
		strings.write_rune(&b, ' ')
	}
	for _ in 0..<token.len {
		strings.write_rune(&b, '^')
	}

	text = strings.to_string(b)

	return
}
