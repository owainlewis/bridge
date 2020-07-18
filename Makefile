build:
	@dune build

run: build
	@./_build/default/bin/bridge.exe

repl: build
	@dune utop

run-example: build
 	@_build/default/bin/bridge.exe examples/example.bridge
