const ERROR_STYLE = "color: #eee; background-color: #d10; padding: 2px 4px"

/**
 * @typedef  {object}             Odin_Exports
 * @property {WebAssembly.Memory} memory
 * @property {() => void}         _start
 * @property {() => void}         _end
 * @property {() => number}       default_context_ptr
 */

/**
 * The Odin WebAssembly instance.
 * @typedef  {object}             Wasm_State
 * @property {WebAssembly.Memory} memory
 * @property {Odin_Exports}       exports
 */

/**
 * @param   {string}              path
 * @param   {WebAssembly.Imports} imports
 * @returns {Promise<WebAssembly.WebAssemblyInstantiatedSource>}
 */
export function fetchInstanciateWasm(path, imports) {
	const wasm_fetch = fetch(path)

	return typeof WebAssembly.instantiateStreaming == "function"
		? WebAssembly.instantiateStreaming(wasm_fetch, imports)
		: wasm_fetch
				.then(r => r.arrayBuffer())
				.then(wasm_file => WebAssembly.instantiate(wasm_file, imports))
}

/**
 * Init a wasm instance with exports and memory from instanciated wasm module exports
 *
 * @param {Wasm_State} state
 * @param {WebAssembly.WebAssemblyInstantiatedSource} src_instance
 */
export function initWasmState(state, src_instance) {
	state.exports = /** @type {Odin_Exports} */ (src_instance.instance.exports)
	state.memory = state.exports.memory
}

/**
 * @param   {ArrayBufferLike} buffer
 * @param   {number}          ptr
 * @param   {number}          len
 * @returns {string}
 */
export const load_string_raw = (buffer, ptr, len) => {
	const bytes = new Uint8Array(buffer, ptr, len)
	return new TextDecoder().decode(bytes)
}

/** @returns {void} */
export function eprintln(/** @type {string} */ text) {
	console.log("%c" + text, ERROR_STYLE)
}

let buffer = ""
/** @type {number | null} */
let last_fd = null

/**
 * @param   {number} fd
 * @param   {string} str
 * @returns {void}
 */
function writeToConsole(fd, str) {
	switch (true) {
		// invalid fd
		case fd !== 1 && fd !== 2:
			buffer = ""
			last_fd = null
			throw Error(`Invalid fd (${fd}) to 'write' ${str}`)
		// flush on newline
		case str[str.length - 1] === "\n":
			buffer += str.slice(0, -1)
			fd === 1 ? console.log(buffer) : eprintln(buffer)
			buffer = ""
			last_fd = null
			break
		// flush on fd change
		case last_fd !== fd && last_fd !== null:
			buffer = ""
			last_fd = fd
			break
		// append to buffer
		default:
			buffer += str
			last_fd = fd
	}
}

/** @param {Wasm_State} wasm */
export function makeOdinEnv(wasm) {
	return {
		/**
		 * @param   {number} fd
		 * @param   {number} ptr
		 * @param   {number} len
		 * @returns {void}
		 */
		write: (fd, ptr, len) => {
			const str = load_string_raw(wasm.memory.buffer, ptr, len)
			writeToConsole(fd, str)
		},
		/** @returns {never} */
		trap: () => {
			throw Error()
		},
		/**
		 * @param   {number} ptr
		 * @param   {number} len
		 * @returns {void}
		 */
		alert: (ptr, len) => {
			const str = load_string_raw(wasm.memory.buffer, ptr, len)
			alert(str)
		},
		/** @returns {never} */
		abort: () => {
			throw Error("abort")
		},
		/**
		 * @param   {number} ptr
		 * @param   {number} len
		 * @returns {void}
		 */
		evaluate: (ptr, len) => {
			const str = load_string_raw(wasm.memory.buffer, ptr, len)
			void eval.call(null, str)
		},
		/** @returns {bigint} */
		time_now: () => BigInt(Date.now()),
		/** @returns {number} */
		tick_now: () => performance.now(),
		/**
		 * @param   {number} duration_ms
		 * @returns {void}
		 */
		time_sleep: duration_ms => {
			if (duration_ms > 0) {
				// TODO(bill): Does this even make any sense?
			}
		},
		/**
		 * @param   {number} x
		 * @returns {number}
		 */
		sqrt: Math.sqrt,
		/**
		 * @param   {number} x
		 * @returns {number}
		 */
		sin: Math.sin,
		/**
		 * @param   {number} x
		 * @returns {number}
		 */
		cos: Math.cos,
		/**
		 * @param   {number} x
		 * @param   {number} y
		 * @returns {number}
		 */
		pow: Math.pow,
		/**
		 * @param   {number} x
		 * @param   {number} y
		 * @param   {number} z
		 * @returns {number}
		 */
		fmuladd: (x, y, z) => x * y + z,
		/**
		 * @param   {number} x
		 * @returns {number}
		 */
		ln: Math.log,
		/**
		 * @param   {number} x
		 * @returns {number}
		 */
		exp: Math.exp,
		/**
		 * @param   {number} x
		 * @param   {number} exp
		 * @returns {number}
		 */
		ldexp: (x, exp) => x * Math.pow(2, exp),
		/**
		 * @param   {number} addr
		 * @param   {number} len
		 * @returns {void}
		 */
		rand_bytes: (addr, len) => {
			const view = new Uint8Array(wasm.memory.buffer, addr, len)
			void crypto.getRandomValues(view)
		},
	}
}
