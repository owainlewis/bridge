# Bridge

Bridge is a stack based functional programming language inspired by Joy.

It makes heavy use of combinators to perform programming operations.

## Example Program

```
module example (
    main
);

import qualified prelude as P;

let main =
  "HELLO" "WORLD" concat println
;

main
```

## Getting Started

Start a REPL session with

```sh
➜  bridge git:(master) ✗ make run
Bridge Interpreter. 2020 Owain Lewis
> 10
10
> :exit
Bye ...
```

To run a program from a file

```sh
➜  bridge git:(master) ✗ ./_build/default/bin/bridge.exe bridge/example.bridge
```

