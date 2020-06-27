open Bridge_lib

let result s = Parser.prog Lexer.token (Lexing.from_string s)

let _ = print_endline "OK";
