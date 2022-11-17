open Ast

let parse (s: string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [is_value e] is whether [e] is a value *)
let is_value : expr -> bool = function
  | Int _ | Bool _ -> true
  | Var _ | Let _ | Binop _ | If _ -> false

let empty = []

(** [typeof env e] is the typeof [e] in environment [env].
    That is, it is the [t] such that [env |- e : t].
    Raises: [Failure] if no such type [t] exists. *)
let typeof env e = 
  failwith "TODO"

  (** [typecheck e] is [e] if [e] typechecks, that is, if there extsts a type
      [t] such that [{} |- e : t].
      Raises: [Failure] if [e] does not type check. *)
let typecheck e = 
  ignore (typeof empty e); e
(* it's equivalent to [let _ = typeof empty e in e] *)
  
(** [subst e v x] is [e] with [v] substituted for [x], that
    is [e{v/x}]. *)
let rec subst e v x = match e with
  | Var y -> if x = y then v else e
  | Bool _ -> e
  | Int _ -> e
  | Binop (bop, e1, e2) -> Binop (bop, subst e1 v x, subst e2 v x)
  | Let (y, t, e1, e2) -> 
    let e' = subst e1 v x in
    if y = x then Let (y, t, e', e2) else Let (y, t, e', subst e2 v x)
  | If (e1, e2, e3) -> If (subst e1 v x, subst e2 v x, subst e3 v x)

(** [step] is the [-->] relation, that is, a single step of evaluation. *)
let rec step : expr -> expr = function
  | Int _ | Bool _ -> failwith "Does not step"
  | Var _ -> failwith "Unbound variable"
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 -> step_bop bop e1 e2
  | Binop (bop, e1, e2) when is_value e1 -> Binop (bop, e1, step e2)
  | Binop (bop, e1, e2) when is_value e2 -> Binop (bop, step e1, e2)
  | Binop (_, _, _) -> failwith "Unhandled binop"
  | Let (x, _, e1, e2) when is_value e1 -> subst e2 e1 x
  | Let (x, t, e1, e2) -> Let (x, t, step e1, e2)
  | If (Bool true, e2, _) -> e2
  | If (Bool false, _, e3) -> e3
  | If (Int _, _, _) -> failwith "Guard of if must have type bool"
  | If (e1, e2, e3) -> If(step e1, e2, e3)

(** [step_bop bop v1 v2] implements the primitive operation
    [v1 bop v2]. Requires: [v1] and [v2] are both values. *)
and step_bop bop e1 e2 = match bop, e1, e2 with
  | Add, Int a, Int b -> Int (a + b)
  | Mult, Int a, Int b -> Int (a * b)
  | Leq, Int a, Int b -> Bool (a <= b)
  | _ -> failwith "Operator and operand mismatch"

(** [eval_small e] is the [e -->* v] relation. That is,
    keep applying [step] until a value is produced. *)
let rec eval_small (e : expr) : expr =
  if is_value e then e
  else e |> step |> eval_small
  
(** [eval e] is the [e ==> v] relation. *)
let rec eval (e : expr) : expr = match e with
  | Int _ | Bool _ -> e
  | Var _ -> failwith "Unbound variable"
  | Binop (bop, e1, e2) -> eval_bop bop e1 e2
  | Let (x, _, e1, e2) -> eval_let x e1 e2
  | If (e1, e2, e3) -> eval_if e1 e2 e3

and eval_let x e1 e2 = 
  let v1 = eval e1 in
  let e2' = subst e2 v1 x in
  eval e2'

(** [eval_bop bop e1 e2] is the [e] such that [e1 bop e2 ==> e]. *)
and eval_bop bop e1 e2 = match bop, eval e1, eval e2 with
  | Add, Int a, Int b -> Int (a + b)
  | Mult, Int a, Int b -> Int (a * b)
  | Leq, Int a, Int b -> Bool (a <= b)
  | _ -> failwith "Operator and operand type mismatch"
  
(** [eval_if e1 e2 e3] is the [e] such that [if e1 then e2 else e3 ==> e]. *)
and eval_if e1 e2 e3 = match eval e1 with
  | Bool true -> eval e2
  | Bool false -> eval e3
  | Int _ -> failwith "Guard of if must have type bool"
  | _ -> failwith "precondition violated"

(** [string_of_val v] converts [v] to a string.
    Requires: [v] represents a value. *)
let string_of_val (e: expr) : string =
  match e with
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | _ -> failwith "precondition violated"  

(** [interp s] interprets [s] by lexing and parsing it,
    evaluating it, and covberting the result to a string. *)
let interp (s: string) : string =
  s |> parse |> typecheck |> eval |> string_of_val 