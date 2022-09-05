%{
  open Ast
%}

%token <int> INT
%token EOF
%token PLUS

%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;

expr:
  | i = INT { Int i }
  | e1 = expr; PLUS ; e2 = expr { Binop(Add, e1, e2) }
  ;
  