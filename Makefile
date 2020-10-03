all: clean build test

.PHONY: build
build:
	@echo "Building project..."
	@dune build
	@echo "Done."

.PHONY: run
run: clean build
	@echo "Running bridge application..."
	@./_build/default/bin/bridge.exe

.PHONY: clean
clean:
	@rm -f ./_build/default/bin/bridge.exe

.PHONY: test
test:
	@echo "Running tests..."
	@dune test
	@echo "Done."

.PHONY: repl
repl: build
	@dune utop

example: build
	@_build/default/bin/bridge.exe examples/example.bridge
