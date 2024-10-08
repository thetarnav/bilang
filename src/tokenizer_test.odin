package bilang

import "core:log"
import "core:testing"

Expect_Tokens_Case :: struct {
	name    : string,
	src     : string,
	expected: []struct {kind: Token_Kind, text: string},
}

expected_list := []Expect_Tokens_Case {
	{	"empty",
		"",
		{},
	},
	/*
	Punctuators
	*/
	{	"Paren_L",
		"(",
		{{.Paren_L, "("}},
	},
	{	"Paren_R",
		")",
		{{.Paren_R, ")"}},
	},
	/*
	Operators
	*/
	{	"Equals",
		"=",
		{{.Eq, "="}},
	},
	{	"Add",
		"+",
		{{.Add, "+"}},
	},
	{	"Pow",
		"x^2",
		{{.Ident, "x"}, {.Pow, "^"}, {.Num, "2"}},
	},
	/*
	Names
	*/
	{	"Name",
		"foo",
		{{.Ident, "foo"}},
	},
	{	"Name with underscore",
		"foo_bar",
		{{.Ident, "foo_bar"}},
	},
	{	"Name with digits",
		"foo123",
		{{.Ident, "foo123"}},
	},
	{	"Name with digits and underscore",
		"foo_123",
		{{.Ident, "foo_123"}},
	},
	{	"Invalid Name",
		"123foo",
		{{.Invalid, "123"}, {.Ident, "foo"}},
	},
	/*
	Int and Float
	*/
	{	"zero",
		"0",
		{{.Num, "0"}},
	},
	{	"int",
		"123",
		{{.Num, "123"}},
	},
	{	"float",
		"123.456",
		{{.Num, "123.456"}},
	},
	{	"invalid float",
		"123.",
		{{.Invalid, "123."}},
	},
	{	"float zero",
		"0.456",
		{{.Num, "0.456"}},
	},
	{	"negative int",
		"-123",
		{{.Sub, "-"}, {.Num, "123"}},
	},
	{	"negative float",
		"-123.456",
		{{.Sub, "-"}, {.Num, "123.456"}},
	},
	{	"negative float zero",
		"-0.456",
		{{.Sub, "-"}, {.Num, "0.456"}},
	},
	/*
	Comments
	*/
	{	"Comment",
		"# foo",
		{},
	},
	{	"Comment with int after LF",
		"# foo\n123",
		{{.EOL, "\n"}, {.Num, "123"}},
	},
	{	"Comment with int after CRLF",
		"# foo\r\n123",
		{{.EOL, "\n"}, {.Num, "123"}},
	},
	/*
	Code Snippets
	*/
	{	"Query Example",
`
a     = -69.5 # a is -69.5
a + b = c + 89 * - 2
a - b = 10 * (5 + 15) / 2
`,     {
			{.EOL,     "\n"},
			{.Ident,   "a"},
			{.Eq,      "="},
			{.Sub,     "-"},
			{.Num,     "69.5"},
			{.EOL,     "\n"},
			{.Ident,   "a"},
			{.Add,     "+"},
			{.Ident,   "b"},
			{.Eq,      "="},
			{.Ident,   "c"},
			{.Add,     "+"},
			{.Num,     "89"},
			{.Mul,     "*"},
			{.Sub,     "-"},
			{.Num,     "2"},
			{.EOL,     "\n"},
			{.Ident,   "a"},
			{.Sub,     "-"},
			{.Ident,   "b"},
			{.Eq,      "="},
			{.Num,     "10"},
			{.Mul,     "*"},
			{.Paren_L, "("},
			{.Num,     "5"},
			{.Add,     "+"},
			{.Num,     "15"},
			{.Paren_R, ")"},
			{.Div,     "/"},
			{.Num,     "2"},
			{.EOL,     "\n"},
		},
	},
}

@(test)
test_tokenizer_cases :: proc(t: ^testing.T)
{
	context.allocator = context.temp_allocator

	tokens := make([dynamic]Token, 0, 10)

	failed_count: int

	for test_case in expected_list {

		tokenizer := make_tokenizer(test_case.src)

		for token in next_token(&tokenizer) {
			append(&tokens, token)
		}
		defer clear_dynamic_array(&tokens)

		good := true

		if len(tokens) != len(test_case.expected) {
			log.errorf(
				"\n\e[0;32m%q\e[0m:\e[0;31m\n\texpected %d tokens, got %d\n\e[0m",
				test_case.name, len(test_case.expected), len(tokens),
			)
			good = false
		}

		for token, i in tokens {
			expected := test_case.expected[i]
			actual   := token.text
			if !(actual == expected.text && token.kind == expected.kind) {
				log.errorf(
					"\n\e[0;32m%q\e[0m:\e[0;31m\nexpected tokens[%d] to be %s `%s`, got %s `%s`\n\e[0m%s",
					test_case.name, i, expected.kind, expected.text, token.kind, actual, display_token_in_line(test_case.src, token),
				)
				good = false
			}
		}

		if !good {
			failed_count += 1
			continue
		}
	}

	if failed_count > 0 {
		log.errorf(
			"\e[0;31mFailed %d cases\e[0m",
			failed_count,
		)
	}
}
