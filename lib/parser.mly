%{
  open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> IDENT
%token SEMICOLON
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token EQUAL
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
| expr               { St_expr $1 }
| LET k = IDENT EQUAL vs = exprs SEMICOLON { St_assign $1 $3 }
;

exprs:
| { [] }
| exprs expr { $2 :: $1 }
;

expr:
| i = INT
    { Expr_int i }
| f = FLOAT
    { Expr_float f }
| id = IDENT
    { Expr_id id }
| s = STRING
    { Expr_string s }
| LBRACKET es = exprs RBRACKET
    { Expr_list (List.rev es) }
;
