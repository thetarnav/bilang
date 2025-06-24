package cli

import "core:fmt"
import "core:os"
import "core:mem"

import bilang "src"


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

	// Read from stdin using a buffer approach
    // @static buffer: [4096]byte
    // total_bytes: [dynamic]byte
    // defer delete(total_bytes)
    
    // for {
    //     n, err := os.read(os.stdin, buffer[:])
    //     if err != os.ERROR_NONE || n == 0 do break
    //     append(&total_bytes, ..buffer[:n])
    // }
    // language_input := string(total_bytes[:])

    language_input := `
    a = 1
    b = 2
    c = a + b
    `

	parser_scratch: mem.Scratch_Allocator
	mem.scratch_allocator_init(&parser_scratch, 1024)

	decls, err := bilang.parse_src(language_input, mem.scratch_allocator(&parser_scratch))

	if err != nil {
		fmt.print(bilang.parser_error_to_string(language_input, err, context.temp_allocator))
		os.exit(1)
	}

	bilang.print_exprs(decls)

	constrs, solved, ok := bilang.resolve(bilang.constraints_from_expr(decls))

	fmt.print("\n-------\n\n")
	bilang.print_constraints(constrs)

	if ok {
		fmt.print("\n--- Solved ---\n\n")
	} else {
		fmt.print("\n--- Could not solve ---\n")
	}

	mem.scratch_allocator_destroy(&parser_scratch)
}
