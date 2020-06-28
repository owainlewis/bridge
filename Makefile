build:
	@dune build

run:
	@./_build/default/bin/bridge.exe

repl: build
	@dune utop
