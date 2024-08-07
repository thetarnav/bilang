package bilang

import "base:intrinsics"
import "core:log"


@(disabled=!ODIN_DEBUG)
log_debug :: #force_inline proc (args: ..any, sep := " ", location := #caller_location)
{
	log.debug(..args, sep=sep, location=location)
}
@(disabled=!ODIN_DEBUG)
log_debugf :: #force_inline proc (fmt_str: string, args: ..any, location := #caller_location)
{
	log.debugf(fmt_str, ..args, location=location)
}
@(disabled=!ODIN_DEBUG)
log_debug_update :: proc (constrs: []Constraint, title := "updated", location := #caller_location)
{
	output, err := contraints_to_string(constrs)

	if err != nil {
		log_debugf("%s: failed to print constraints: %v", title, err, location=location)
	} else {
		log_debugf("%s:\n%s", title, output, location=location)
	}
}

@(require_results)
slice_equals_by :: proc(a, b: $T/[]$E, f: proc (E, E) -> bool) -> bool #no_bounds_check
{
	if len(a) != len(b) do return false
	for i in 0..<len(a) {
		if f(a[i], b[i]) do return false
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
