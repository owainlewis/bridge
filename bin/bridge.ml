open Bridge_lib

let parse_string s = Parser.prog Lexer.token (Lexing.from_string s)

let _ =
  try
    let prog = parse_string "1 ;" in
    print_endline(Ast.to_string prog);
  with
  | Lexer.Error msg ->
    Printf.eprintf "%s%!" msg
  (* TODO get the error here *)
  | Parser.Error ->
    Printf.eprintf "syntax error.\n%!"
    ;
