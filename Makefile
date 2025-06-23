PROD_ARGS = -o:aggressive -disable-assert -no-bounds-check -obfuscate-source-code-locations
ERROR_POS_STYLE = -error-pos-style:unix

# Capture additional arguments (excluding the target name)
EXTRA_ARGS = $(filter-out $@,$(MAKECMDGOALS))

# Default target
.PHONY: help
help:
	@echo "Available targets:"
	@echo "  build-test      - Build test binary"
	@echo "  test           - Run tests"
	@echo "  build-wasm     - Build WebAssembly version"
	@echo "  build-cli-prod - Build CLI production version"
	@echo "  build-cli-debug- Build CLI debug version"
	@echo "  clean          - Clean build artifacts"
	@echo ""
	@echo "You can pass additional arguments: make test -- -debug"

# Build test binary
.PHONY: build-test
build-test:
	@echo "Building test"
	odin build src \
		$(ERROR_POS_STYLE) \
		-debug \
		-build-mode:test \
		-out:test.bin \
		$(EXTRA_ARGS)

# Run tests
.PHONY: test
test:
	@echo "Running test"
	odin test src \
		$(ERROR_POS_STYLE) \
		-define:ODIN_TEST_LOG_LEVEL=warning \
		-define:ODIN_TEST_FANCY=false \
		-out:test.bin \
		$(EXTRA_ARGS)

# Build WebAssembly version
.PHONY: build-wasm
build-wasm:
	@echo "Building wasm"
	odin build site \
		-target:js_wasm32 \
		-out:site/_main.wasm \
		$(ERROR_POS_STYLE) \
		$(PROD_ARGS) \
		$(EXTRA_ARGS)

# Build CLI production version
.PHONY: build-cli-prod
build-cli-prod:
	@echo "Building cli prod"
	odin build . \
		-out:bilang \
		$(ERROR_POS_STYLE) \
		$(PROD_ARGS) \
		$(EXTRA_ARGS)

# Build CLI debug version
.PHONY: build-cli-debug
build-cli-debug:
	@echo "Building cli debug"
	odin build . \
		-out:bilang \
		$(ERROR_POS_STYLE) \
		-debug \
		$(EXTRA_ARGS)

# Clean build artifacts
.PHONY: clean
clean:
	@echo "Cleaning build artifacts"
	rm -f test.bin bilang site/_main.wasm

# Prevent make from trying to build files named after extra arguments
%:
	@:
