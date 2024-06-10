import * as odin_env from "./odin_env.js"

/**
 * @typedef  {object}                   Example_Exports
 * @property {Example_Start           } start
 * @property {Example_Frame           } frame
 * @property {Example_On_Window_Resize} on_window_resize
 *
 * @typedef {odin_env.Odin_Exports & Example_Exports} Wasm_Exports
 *
 * @callback Example_Start
 * @param   {number} ctx_ptr
 * @returns {number}
 *
 * @callback Example_Frame
 * @param   {number} ctx_ptr
 * @param   {number} delta
 * @returns {void  }
 *
 * @callback Example_On_Window_Resize
 * @param   {number} window_w
 * @param   {number} window_h
 * @param   {number} canvas_w
 * @param   {number} canvas_h
 * @param   {number} canvas_x
 * @param   {number} canvas_y
 * @returns {void  }
 */

/** @type {odin_env.Wasm_State} */
const wasm_state  = {
	memory : /** @type {*} */ (null),
	exports: /** @type {*} */ (null),
}

const src_instance = await odin_env.fetchInstanciateWasm("_main.wasm", {
	env: {}, // TODO
	odin_env: odin_env  .makeOdinEnv    (wasm_state),
})

odin_env.initWasmState(wasm_state, src_instance)
const exports = /** @type {Wasm_Exports} */ (wasm_state.exports)

console.log("WASM exports:", exports)
console.log("WASM memory:", exports.memory)

/*
Main
*/

// exports._start() // Calls main
// const odin_ctx = exports.default_context_ptr()
/* _end() should be called when the program is done */
// exports._end()

// const ok = exports.start(odin_ctx)
// if (!ok) throw Error("Failed to start example")

// void requestAnimationFrame(prev_time => {
// 	/** @type {FrameRequestCallback} */
// 	const frame = time => {
// 		const delta = time - prev_time
// 		prev_time = time
// 		exports.frame(odin_ctx, delta)
// 		void requestAnimationFrame(frame)
// 	}

// 	void requestAnimationFrame(frame)
// })
