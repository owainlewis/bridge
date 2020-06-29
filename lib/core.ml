let parse_string: string -> Ast.t = fun s ->
  Parser.prog Lexer.token (Lexing.from_string s)

let parse_file filename =
  let channel = open_in filename in
  Parser.prog Lexer.token (Lexing.from_channel channel)

let parse_program_string: string -> Ast.t = fun s ->
  try
    parse_string s
  with
  | Lexer.Error (msg, pos) ->
    let _ = Printf.eprintf "Unexpected token: %s on line: %d" msg pos.pos_lnum in
    [];;
