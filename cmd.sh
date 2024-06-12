#!/bin/bash

case "$1" in
	test)
		echo "Running test"
		# -define:ODIN_TEST_NAMES=bilang.test_solver
		odin test src \
			-error-pos-style:unix \
			-define:ODIN_TEST_LOG_LEVEL=warning \
			-define:ODIN_TEST_FANCY=false
		;;
	build_wasm)
		echo "Building wasm"
		odin build site \
			-target:js_wasm64p32 \
			-out:site/_main.wasm \
			-error-pos-style:unix
		;;
	*)
		# Default case when no matching script is found
		echo "Invalid argument. Please provide a valid script name."
		echo "Available scripts: test, build_wasm"
		exit 1
		;;
esac