build:
	@dune build

run: build
	@./_build/default/bin/bridge.exe

repl: build
	@dune utop
