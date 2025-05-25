package bilang

import "../utils"

assert_equal :: utils.assert_equal
is_int       :: utils.is_int

@(require_results)
pow_int :: proc "contextless" (base, exp: int) -> (result: int, ok: bool) {

    if exp < 0   do return 0, false
    if exp == 0  do return 1, true
    if base == 0 do return 0, true
    if base == 1 do return 1, true
    if base == -1 {
        return -1 if exp % 2 == 1 else 1, true // (-1)^odd = -1, (-1)^even = 1
    }
    
    // Check for potential overflow early
    if exp > 63 && abs(base) > 1 do return 0, false
    
    result = 1
    current_base := base
    remaining_exp := exp
    
    for remaining_exp > 0 {
        if remaining_exp % 2 == 1 {
            if current_base > 0 && result > max(int) / current_base                 do return 0, false
            if current_base < 0 && (result < 0 && result < max(int) / current_base) do return 0, false
            if current_base < 0 && (result > 0 && result > min(int) / current_base) do return 0, false
            
            result *= current_base
        }
        
        remaining_exp /= 2
        if remaining_exp > 0 {
            if current_base > 0 && current_base > max(int) / current_base    do return 0, false
            if current_base < 0 && current_base < max(int) / (-current_base) do return 0, false
            
            current_base *= current_base
        }
    }
    
    return result, true
}
