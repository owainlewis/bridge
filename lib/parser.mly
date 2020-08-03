%{
  open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> IDENTIFIER
%token SEMICOLON
%token LEFT_BRACKET RIGHT_BRACKET
%token EQUALS
%token TRUE
%token FALSE
%token MODULE
%token LET
%token EOF

%start prog

%type <Ast.t> prog

%%

prog:
| statements EOF { List.rev $1 }
;

statements:
| { [] }
| statements statement { $2 :: $1 }
;

statement:
| expr
  { St_expr $1 }
| LET k = IDENTIFIER EQUALS vs = exprs SEMICOLON
  { St_assign(k, vs) }
| MODULE k = IDENTIFIER SEMICOLON
  { St_module(k) }
;

exprs:
| { [] }
| exprs expr { $2 :: $1 }
;

expr:
| TRUE
    { Expr_bool true }
| FALSE
    { Expr_bool false }
| i = INT
    { Expr_int i }
| f = FLOAT
    { Expr_float f }
| id = IDENTIFIER
    { Expr_id id }
| s = STRING
    { Expr_string s }
| LEFT_BRACKET es = exprs RIGHT_BRACKET
    { Expr_list (List.rev es) }
;
