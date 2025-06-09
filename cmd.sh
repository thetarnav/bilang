#!/bin/bash

prod_args=(
	"-o:aggressive"
	"-disable-assert"
	"-no-bounds-check"
	"-obfuscate-source-code-locations"
)

case "$1" in
	build_test)
		echo "Building test"
		shift
		odin build src \
			-error-pos-style:unix \
			-debug \
			-build-mode:test \
			-out:test.bin \
			"$@"
		;;
	test)
		echo "Running test"
		shift
		# -define:ODIN_TEST_NAMES=bilang.test_solver
		odin test src \
			-error-pos-style:unix \
			-define:ODIN_TEST_LOG_LEVEL=warning \
			-define:ODIN_TEST_FANCY=false \
			-out:test.bin \
			"$@"
		;;
	build_wasm)
		echo "Building wasm"
		shift
		odin build site \
			-target:js_wasm32 \
			-out:site/_main.wasm \
			-error-pos-style:unix \
			"${prod_args[@]}" \
			"$@"
		;;
	build_cli_prod)
		echo "Building cli prod"
		shift
		odin build . \
			-out:bilang \
			-error-pos-style:unix \
			"${prod_args[@]}" \
			"$@"
		;;
	build_cli_debug)
		echo "Building cli debug"
		shift
		odin build . \
			-out:bilang \
			-error-pos-style:unix \
			-debug \
			"$@"
		;;
	*)
		# Default case when no matching script is found
		echo "Invalid argument. Please provide a valid script name."
		echo "Available scripts: test, build_wasm, build_cli_prod, build_cli_debug, build_test"
		exit 1
		;;
esac