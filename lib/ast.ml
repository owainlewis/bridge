open Printf

type token =
  | INT of int
  | FLOAT of float
  | STRING of string
  | IDENT of string
  | LET
  | MODULE
  | SEMICOLON
  | LBRACKET
  | RBRACKET
  | EQUAL
  | EOF

type t = statement list

and statement =
  | St_expr of expr
  | St_assign of string * expr list
  | St_module of string

and expr =
  | Expr_int of int
  | Expr_float of float
  | Expr_string of string
  | Expr_id of string
  | Expr_list of expr list

let rec expr_to_string = function
  | Expr_int i -> string_of_int i
  | Expr_float f -> string_of_float f
  | Expr_string s -> sprintf "'%s'" s
  | Expr_id id -> id
  | Expr_list vs ->
    let inner_forms =
      String.concat " " (List.map expr_to_string vs) in
    "[" ^ inner_forms ^ "]"

let statement_to_string = function
  | St_expr e -> (expr_to_string e)
  | St_assign (k, vs) -> k ^ " = " ^ (String.concat " " (List.map expr_to_string vs))
  | St_module k -> "module: " ^ k

let to_string prog =
  String.concat "\n" (List.map statement_to_string prog)
