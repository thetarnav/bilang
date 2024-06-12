import * as odin_env from "./odin_env.js"


/**
 * @typedef  {object} Example_Exports
 * @property {(len: bigint) => void} solve
 * @property {() => number}          get_input_ptr
 * @property {() => bigint}          get_input_len
 *
 * @typedef {odin_env.Odin_Exports & Example_Exports} Wasm_Exports
 */


const textarea_input  = /** @type {HTMLTextAreaElement} */ (document.getElementById("input"))
const textarea_output = /** @type {HTMLTextAreaElement} */ (document.getElementById("output"))

/** @type {odin_env.Wasm_State} */
const wasm_state  = {
	memory : /** @type {*} */ (null),
	exports: /** @type {*} */ (null),
}

const src_instance = await odin_env.fetchInstanciateWasm("_main.wasm", {
	env: {
		/**
		 * @param {number} ptr 
		 * @param {bigint} len
		 */
		output(ptr, len) {
			let output = odin_env.load_string_raw(wasm_state.memory.buffer, ptr, len)
			console.log("Output:", output)
			textarea_output.value = output
		},
	}, // TODO
	odin_env: odin_env  .makeOdinEnv    (wasm_state),
})

odin_env.initWasmState(wasm_state, src_instance)
const exports = /** @type {Wasm_Exports} */ (wasm_state.exports)

console.log("WASM exports:", exports)
console.log("WASM memory:", exports.memory)

textarea_input.value =
	"10 = a + b\n"+
	"a = -4 + 2\n"

function on_input() {
	let input = textarea_input.value
	let buffer_ptr = exports.get_input_ptr()
	let buffer_len = exports.get_input_len()
	
	let input_buffer = new Uint8Array(wasm_state.memory.buffer, buffer_ptr, Number(buffer_len))
	let encoded = new TextEncoder().encode(input)
	input_buffer.set(encoded)
	
	exports.solve(BigInt(encoded.length))
}

textarea_input.addEventListener("input", on_input)

on_input()