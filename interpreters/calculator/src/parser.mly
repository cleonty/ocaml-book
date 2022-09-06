%{
  open Ast
%}

%token <int> INT
%token EOF
%token TIMES
%token PLUS

/* both are left assosiative, but TIMES has higher precedence */ 
%left PLUS
%left TIMES

%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;

expr:
  | i = INT { Int i }
  | e1 = expr; TIMES ; e2 = expr { Binop(Mult, e1, e2) }
  | e1 = expr; PLUS ; e2 = expr { Binop(Add, e1, e2) }
  ;
  