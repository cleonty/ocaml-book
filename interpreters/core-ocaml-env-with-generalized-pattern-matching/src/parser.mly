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
%token FUN ARROW LPAREN RPAREN EOF TRUE FALSE PLUS MINUS TIMES DIV LEQ LE GEQ GE EQUALS LET IN IF THEN ELSE FIRST SECOND COMMA LEFT RIGHT MATCH WITH ALT UNDERSCORE

%nonassoc IN
%nonassoc ELSE
%nonassoc ARROW
%right FIRST SECOND
%right LEFT RIGHT
%left LEQ LE GEQ GE
%left EQUALS
%left PLUS MINUS
%left TIMES DIV

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
	| e1 = simpl_expr; DIV; e2 = simpl_expr { Binop (Div, e1, e2) }
  | e1 = simpl_expr; PLUS; e2 = simpl_expr { Binop (Add, e1, e2) }
  | e1 = simpl_expr; MINUS; e2 = simpl_expr { Binop (Sub, e1, e2) }
  | e1 = simpl_expr; LE; e2 = simpl_expr { Binop (Le, e1, e2) }
  | e1 = simpl_expr; LEQ; e2 = simpl_expr { Binop (Leq, e1, e2) }
  | e1 = simpl_expr; GE; e2 = simpl_expr { Binop (Ge, e1, e2) }
  | e1 = simpl_expr; GEQ; e2 = simpl_expr { Binop (Geq, e1, e2) }
  | e1 = simpl_expr; EQUALS; e2 = simpl_expr { Binop (Equals, e1, e2) }
	| LET; x = ID; EQUALS; e1 = simpl_expr; IN; e2 = simpl_expr { Let (x, e1, e2) }
	| IF; e1 = simpl_expr; THEN; e2 = simpl_expr; ELSE; e3 = simpl_expr { If (e1, e2, e3) }
	| LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN { Pair (e1, e2) } 
	| FIRST; e = simpl_expr { Fst (e) } 
	| SECOND; e = simpl_expr { Snd (e) }
	| LEFT; e = simpl_expr { Left (e) }
	| RIGHT; e = simpl_expr { Right (e) }
	| MATCH; e = simpl_expr; WITH; LEFT; x1 = ID; ARROW; e1 = simpl_expr; ALT; RIGHT; x2 = ID; ARROW; e2 = simpl_expr;  { Match(e, x1, e1, x2, e2, None) }
	| MATCH; e = simpl_expr; WITH; LEFT; x1 = ID; ARROW; e1 = simpl_expr; ALT; RIGHT; x2 = ID; ARROW; e2 = simpl_expr; ALT; UNDERSCORE; e3 = simpl_expr { Match(e, x1, e1, x2, e2, Some e3) }
  | LPAREN; e=expr; RPAREN { e } 
  | LPAREN; MINUS; e=expr; RPAREN { Unop (Uminus, e) } 
  | LPAREN; PLUS; e=expr; RPAREN { Unop (Uplus, e) }
  ;
