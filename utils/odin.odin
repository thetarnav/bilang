package utils

import "base:intrinsics"
import "base:runtime"

import "core:fmt"

@require_results
slice_equals_by :: proc(a, b: $T/[]$E, f: proc (E, E) -> bool) -> bool #no_bounds_check
{
	if len(a) != len(b) {
		return false
	}
	for i in 0..<len(a) {
		if f(a[i], b[i]) {
			return false
		}
	}
	return true
}

@require_results
array_cast :: proc "contextless" (v: $A/[$N]$E, $T: typeid) -> (w: T)
	where intrinsics.type_is_array(T), len(T) == N #no_bounds_check
{
	for i in 0..<N {
		w[i] = cast(intrinsics.type_elem_type(T))(v[i])
	}
	return
}

@(disabled=ODIN_DISABLE_ASSERT)
assert_equal :: proc (a, b: $T, message := "value assertion", loc := #caller_location)
	where intrinsics.type_is_comparable(T)
{
	if a != b {
		assert(false, fmt.tprintf("%s: %v != %v", message, a, b), loc)
	}
}

is_int :: #force_inline proc (float: f64) -> bool {
	return f64(int(float)) == float
}

alloc_error_message :: proc ($PREFIX: string, err: runtime.Allocator_Error) -> string
{
	switch err {
	case .Invalid_Argument:     return PREFIX+"Invalid_Argument"
	case .Invalid_Pointer:      return PREFIX+"Invalid_Pointer"
	case .Mode_Not_Implemented: return PREFIX+"Mode_Not_Implemented"
	case .Out_Of_Memory:        return PREFIX+"Out_Of_Memory"
	case .None:                 return PREFIX+"None"
	case:                       return PREFIX+"Unknown"
	}
}

@(disabled=ODIN_DISABLE_ASSERT)
alloc_error_assert :: proc ($PREFIX: string, err: runtime.Allocator_Error, loc := #caller_location) {
	if err != nil {
		panic(alloc_error_message("atom_new error: ", err), loc)
	}
}
