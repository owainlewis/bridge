open Bridge_lib

let parse_string s = Parser.prog Lexer.token (Lexing.from_string s)

let _ =
  let result = parse_string "1" in
  match result with
  | [] -> print_endline "[]"
  | x::_-> print_endline (Ast.statement_to_string x)
