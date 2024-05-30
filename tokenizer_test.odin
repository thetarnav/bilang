package bilang

import "core:fmt"
import test "core:testing"

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
		{{.Num, "123"}},
	},
	{	"Comment with int after CRLF",
		"# foo\r\n123",
		{{.Num, "123"}},
	},
	/*
	Code Snippets
	*/
	{	"Query Example",
`
a     = -69.5
a + b = c + 89 * - 2
a - b = 10 * (5 + 15) / 2
`,     {
			{.Ident,   "a"},
			{.Eq,      "="},
			{.Sub,     "-"},
			{.Num,     "69.5"},
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
		},
	},
}

test_only_name: string

@(test)
test_tokenizer_cases :: proc(t: ^test.T) {
	tokens := make([dynamic]Token, 0, 10)

	failed_count: int

	for test_case in expected_list {
		switch test_only_name {
		case "", test_case.name:
		case:
			failed_count += 1
			continue
		}

		tokenizer := make_tokenizer(test_case.src)

		for token in next_token(&tokenizer) {
			append(&tokens, token)
		}
		defer clear_dynamic_array(&tokens)

		good := test.expectf(t,
			len(tokens) == len(test_case.expected),
			"\n\e[0;32m%q\e[0m:\e[0;31m\n\texpected %d tokens, got %d\n\e[0m",
			test_case.name, len(test_case.expected), len(tokens),
		)

		for token, i in tokens {
			token_good := test.expectf(t,
				token_string(test_case.src, token) == test_case.expected[i].text &&
				token.kind == test_case.expected[i].kind,
				"\n\e[0;32m%q\e[0m:\e[0;31m\n\texpected tokens[%d] to be %v, got %v\n\e[0m",
				test_case.name, i, test_case.expected[i], token,
			)
			good = good && token_good
		}

		if !good {
			failed_count += 1
			continue
		}
	}

	if failed_count > 0 {
		test.errorf(t, "\e[0;31mFailed %d cases\e[0m", failed_count)
	} else {
		test.logf(t, "\e[0;32mAll cases passed\e[0m")
	}
}
