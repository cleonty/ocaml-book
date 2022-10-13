(* This file uses some advanced parsing techniques
   to parse juxtaposed applications [e1 e2 e3] the
	 same way as OCaml does. *)

%{
open Ast

(** [make_apply e [e1; e2; ...]] makes the application  
    [e e1 e2 ...]).  Requires: the list argument is non-empty. *)
let rec make_apply e = function
  | [] -> failwith "precondition violated"
  | [e'] -> App (e, e')
	| h :: ((_ :: _) as t) -> make_apply (App (e, h)) t
%}

%token <string> ID
%token <int> INT
%token FUN ARROW LPAREN RPAREN EOF TRUE FALSE PLUS TIMES

%left PLUS
%left TIMES

%start <Ast.expr> prog

%%

prog:
	| e = expr; EOF { e }
	;
	
expr:
	| e = simpl_expr { e }
	| e = simpl_expr; es = simpl_expr+ { make_apply e es }
	| FUN; x = ID; ARROW; e = expr { Fun (x, e) }
	;

simpl_expr:
	| x = ID { Var x }
	| x = INT { Int x }
	| TRUE { Bool true }
	| FALSE { Bool false }
	| e1 = simpl_expr; TIMES; e2 = simpl_expr { Binop (Mult, e1, e2) }
  | e1 = simpl_expr; PLUS; e2 = simpl_expr { Binop (Add, e1, e2) }
  | LPAREN; e=expr; RPAREN { e } 
  ;
