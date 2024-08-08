package utils

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
