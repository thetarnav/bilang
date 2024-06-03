package bilang

import "core:fmt"
import "core:os"
import "core:mem"


main :: proc ()
{
	track: mem.Tracking_Allocator
    mem.tracking_allocator_init(&track, context.allocator)
    context.allocator = mem.tracking_allocator(&track)

    defer {
        if len(track.allocation_map) > 0 {
            fmt.eprintf("\n\e[0;31m=== %v allocations not freed ===\e[0m\n", len(track.allocation_map))
            for _, entry in track.allocation_map {
                fmt.eprintf("- % 4d bytes @ %v\n", entry.size, entry.location)
            }
        }
        if len(track.bad_free_array) > 0 {
            fmt.eprintf("\n\e[0;31m=== %v incorrect frees ===\e[0m\n", len(track.bad_free_array))
            for entry in track.bad_free_array {
                fmt.eprintf("- %p @ %v\n", entry.memory, entry.location)
            }
        }
        mem.tracking_allocator_destroy(&track)
    }


	language_input := `
a + b = 10
a     = -4 + 2
`

	parser_scratch: mem.Scratch_Allocator
	mem.scratch_allocator_init(&parser_scratch, 1024)

	decls, err := parse_src(language_input, mem.scratch_allocator(&parser_scratch))

	if err != nil {
		fmt.print(parser_error_to_string(language_input, err, context.temp_allocator))
		os.exit(1)
	}

	print_decls(decls)

	constraints := solve(decls)

	fmt.print("\n-------\n\n")
	print_contraints(constraints)

	mem.scratch_allocator_destroy(&parser_scratch)
}
