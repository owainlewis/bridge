build:
	@dune build

run: build
	@./_build/default/bin/bridge.exe

clean:
	@rm -f ./_build/default/bin/bridge.exe

repl: build
	@dune utop

example: build
	@_build/default/bin/bridge.exe examples/example.bridge
