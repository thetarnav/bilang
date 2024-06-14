package bilang


@(require_results)
slice_equals_by :: proc(a, b: $T/[]$E, f: proc (E, E) -> bool) -> bool #no_bounds_check
{
	if len(a) != len(b) do return false
	for i in 0..<len(a) {
		if f(a[i], b[i]) do return false
	}
	return true
}
