package bilang

import "core:strings"
import "core:fmt"
import "core:mem"
import test "core:testing"


@test test_solver :: proc (t: ^test.T)
{
	cases := []struct {
		input   : string,
		expected: string,
	}{
		{
			"a + b = 10"+"\n"+
			"a = -4 + 2",

			"b = 12"+"\n"+
			"a = -2",	
		}
	}

	parser_scratch: mem.Scratch_Allocator
	mem.scratch_allocator_init(&parser_scratch, 1024, context.temp_allocator)
	scratch_allocator := mem.scratch_allocator(&parser_scratch)

	for test_case in cases {
		context.temp_allocator = scratch_allocator
		context.allocator = scratch_allocator

		defer free_all(scratch_allocator)


		decls, err := parse_src(test_case.input)
		
		if err != nil {
			test.errorf(t,
				"\nFailed to parse input:\n%s",
				parser_error_to_string(test_case.input, err),
			)
			continue
		}
	
		constraints := solve(decls)

		fmt.println(constraints)
	
		b := strings.builder_make_len_cap(0, 1024)
		w := strings.to_writer(&b)
	
		write_contraints(w, constraints, false)
	
		output := strings.to_string(b)
	
		test.expectf(t,
			output == test_case.expected,
			"\nEXPECTED:\n%s\nACTUAL:\n%s",
			test_case.expected, output,
		)
	}
}
