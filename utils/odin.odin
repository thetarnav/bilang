package utils

import "base:intrinsics"

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
