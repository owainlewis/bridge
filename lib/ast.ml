open Printf

type token =
  | INT of int
  | FLOAT of float
  | STRING of string
  | IDENT of string
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | EOF

type t = statement list

and statement =
  | St_expr of expr
  | St_assign of string * expr

and expr =
  | Expr_int of int
  | Expr_float of float
  | Expr_string of string
  | Expr_id of string

let expr_to_string = function
  | Expr_id id -> id
  | Expr_string s -> sprintf "'%s'" s
  | Expr_int i -> string_of_int i
  | Expr_float f -> string_of_float f

let statement_to_string = function
  | St_expr e -> (expr_to_string e)
  | St_assign (k, v) -> k ^ " = " ^ (expr_to_string v)

let to_string prog =
  String.concat "\n" (List.map statement_to_string prog)
