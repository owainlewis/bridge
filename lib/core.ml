let parse_with_error lexbuf =
  try Parser.prog Lexer.token lexbuf
  with
  | Parser.Error ->
    let pos = lexbuf.lex_curr_p in
    Printf.eprintf "Parse error on line: %d\n" pos.pos_lnum;
    []
  | Lexer.Error (msg, pos) ->
    Printf.eprintf "Unexpected token: %s on line: %d\n" msg pos.pos_lnum;
    []

let parse_string: string -> Ast.t = fun s ->
  parse_with_error (Lexing.from_string s)

let parse_file filename =
  let channel = open_in filename in
  parse_with_error (Lexing.from_channel channel)

let run program =
  let ast = parse_string program
  and initial_state = Interpreter.mk_state() in
  Interpreter.interpret initial_state ast
