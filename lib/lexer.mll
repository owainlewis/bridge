{
  open Ast

  exception Error of string
}

rule token = parse
| [' ' '\t' '\n']+ { token lexbuf }
| ';' { SEMICOLON }
| '(' { LPAREN }
| ')' { RPAREN }
| '[' { LBRACKET }
| ']' { RBRACKET }

| ['0'-'9']+ as i
    { INT (int_of_string i) }

| ['0'-'9']+ '.' ['0'-'9']* as f
    { FLOAT (float_of_string f) }

| ['A'-'Z''a'-'z''0'-'9''_']['A'-'Z''a'-'z''0'-'9''-''_']* as id { IDENT(id) }

| '"' [^ '"']* "\""
    { let s = Lexing.lexeme lexbuf in
      STRING(String.sub s 1 (String.length s - 2)) }

| eof
    { EOF }

| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
