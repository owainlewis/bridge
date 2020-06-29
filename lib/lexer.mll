{
  open Ast

  exception Error of string * Lexing.position

  let lexing_error lexbuf =
    let invalid_input = String.make 1 (Lexing.lexeme_char lexbuf 0) in
    raise (Error (invalid_input, lexbuf.Lexing.lex_curr_p))
}

let white   = [' ' '\t' '\n' '\r']+
let newline = '\n' | '\r' | "\r\n"

rule token = parse
(* skip whitespace *)
| white    { token lexbuf }
| newline  { token lexbuf }
| ";"      { SEMICOLON    }
| "["      { LBRACKET     }
| "]"      { RBRACKET     }
| "="      { EQUAL        }
| "let"    { LET          }
| "module" { MODULE       }

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

| _ { lexing_error lexbuf }
