package bilang

import "../utils"

assertf      :: utils.assertf
assert_equal :: utils.assert_equal

is_int :: #force_inline proc (float: f64) -> bool {
	return f64(int(float)) == float
}
