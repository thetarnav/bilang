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
