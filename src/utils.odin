package bilang

import "base:runtime"
import "../utils"
import "core:strconv"
import "core:strconv/decimal"

assert_equal :: utils.assert_equal
is_int       :: utils.is_int


@require_results
new_val :: proc (
	$T: typeid,
	val: T,
	allocator := context.allocator,
	loc := #caller_location,
) -> (ptr: ^T, err: runtime.Allocator_Error) #optional_allocator_error {
	ptr, err = new(T, allocator, loc)
	if err == nil {
		ptr^ = val
	}
	return
}

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

// From core:strconv
generic_ftoa :: proc(buf: []byte, val: f64, bit_size: int) -> []byte {
	bits: u64
	flt: ^strconv.Float_Info
	switch bit_size {
	case 16:
		bits = u64(transmute(u16)f16(val))
		flt = &strconv._f16_info
	case 32:
		bits = u64(transmute(u32)f32(val))
		flt = &strconv._f32_info
	case 64:
		bits = transmute(u64)val
		flt = &strconv._f64_info
	case:
		panic("strconv: invalid bit_size")
	}

	neg  := bits>>(flt.expbits+flt.mantbits) != 0
	exp  := int(bits>>flt.mantbits) & (1<<flt.expbits - 1)
	mant := bits & (u64(1) << flt.mantbits - 1)

	switch exp {
	case 1<<flt.expbits - 1:
		s: string
		if mant != 0 {
			s = "NaN"
		} else if neg {
			s = "-Inf"
		} else {
			s = "+Inf"
		}
		n := copy(buf, s)
		return buf[:n]

	case 0: // denormalized
		exp += 1

	case:
		mant |= u64(1) << flt.mantbits
	}

	exp += flt.bias

	d_: decimal.Decimal
	d := &d_
	decimal.assign(d, mant)
	decimal.shift(d, exp - int(flt.mantbits))
	digs: strconv.Decimal_Slice

    strconv.round_shortest(d, mant, exp, flt)
    digs = strconv.Decimal_Slice{digits = d.digits[:], count = d.count, decimal_point = d.decimal_point}
    prec := max(digs.count-1, 1)

	return strconv.format_digits(buf, true, neg, digs, prec, 'f')
}
