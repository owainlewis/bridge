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
  | St_assign of lvalue * expr

and expr =
  | Expr_int of int
  | Expr_float of float
  | Expr_string of string
  | Expr_id of string

and lvalue =
  | Lval_id of string
