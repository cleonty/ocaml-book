open Ast

exception TypeError of string
exception RuntimeError of string

let type_error s =
  raise (TypeError s)

let runtime_error s =
  raise (RuntimeError s)

let parse (s: string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [is_value e] is whether [e] is a value *)
let is_value : expr -> bool = function
  | Int _ | Bool _ -> true
  | Var _ | Let _ | Binop _ | If _ -> false

(** The error message produced if a variable is unbound. *)
let unbound_var_err = "Unbound variable"

let bop_err = "Operator and operand mismatch"

(** The error message if a if condition is not boolean. *)
let if_guard_err = "Guard of if must have type bool"
  
(** The error message produced if the two branches of an [if]
    do not have the same type. *)
let if_branch_err = "Branches of if must have same type"

(** The error message produced if the binding expression of a [let]
    does not have the same type as the annotation on the variable name. *)
let annotation_error = "Let expression type mismatch"

(** [empty] is the empty environemnt. *)
let empty = []

(** [lookup env x] is the type of [x] in environment [e].
    Raises: [Failure] if [x] is not bound in [env]. *)
let lookup env x =
  match List.assoc_opt x env with
  | Some t -> t
  | None -> type_error unbound_var_err

(** [extend env x t] is [env] extended with a binding of
    [x] to [t]. *)
let extend env x t =
  (x, t) :: env

(** [typeof env e] is the typeof [e] in environment [env].
    That is, it is the [t] such that [env |- e : t].
    Raises: [Failure] if no such type [t] exists. *)
let rec typeof env = function
  | Bool _ -> TBool
  | Int _ -> TInt
  | Var x -> lookup env x
  | Binop (bop, e1, e2) -> typeof_binop env bop e1 e2
  | Let (x, t, e1, e2) -> typeof_let env x t e1 e2
  | If (e1, e2, e3) -> typeof_if env e1 e2 e3

(** [typeof_binop env bop e1 e2] is the type of [e1 bop e2] in
    environment [env]. *)
and typeof_binop env bop e1 e2 =
  match bop, typeof env e1, typeof env e2 with
  | Add, TInt, TInt -> TInt
  | Mult, TInt, TInt -> TInt
  | Leq, TInt, TInt -> TBool
  | _ -> type_error bop_err

(** [typeof_let env x t e1 e2] is the type of [let x: t = e1 in e2 ]
    in environment [env]. *)
and typeof_let env x t e1 e2 =
    let t' = typeof env e1 in
    if t = t' then
      let env' = extend env x t' in
      typeof env' e2
    else
      type_error annotation_error

(** [typeof_if env e1 e2 e3] is the type of [if e1 then e2 else e3]
    in environemnt [env]. *)
and typeof_if env e1 e2 e3 =
  let t1 = typeof env e1 in
  if t1 <> TBool then
    type_error if_guard_err
  else
    let t2 = typeof env e2 in
    let t3 = typeof env e3 in
    if t2 <> t3 then
      type_error if_branch_err
    else
      t2 
  
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
  | Int _ | Bool _ -> runtime_error "Does not step"
  | Var _ -> runtime_error unbound_var_err
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 -> step_bop bop e1 e2
  | Binop (bop, e1, e2) when is_value e1 -> Binop (bop, e1, step e2)
  | Binop (bop, e1, e2) when is_value e2 -> Binop (bop, step e1, e2)
  | Binop (_, _, _) -> runtime_error "Unhandled binop"
  | Let (x, _, e1, e2) when is_value e1 -> subst e2 e1 x
  | Let (x, t, e1, e2) -> Let (x, t, step e1, e2)
  | If (Bool true, e2, _) -> e2
  | If (Bool false, _, e3) -> e3
  | If (Int _, _, _) -> runtime_error "Guard of if must have type bool"
  | If (e1, e2, e3) -> If(step e1, e2, e3)

(** [step_bop bop v1 v2] implements the primitive operation
    [v1 bop v2]. Requires: [v1] and [v2] are both values. *)
and step_bop bop e1 e2 = match bop, e1, e2 with
  | Add, Int a, Int b -> Int (a + b)
  | Mult, Int a, Int b -> Int (a * b)
  | Leq, Int a, Int b -> Bool (a <= b)
  | _ -> runtime_error "Operator and operand mismatch"

(** [eval_small e] is the [e -->* v] relation. That is,
    keep applying [step] until a value is produced. *)
let rec eval_small (e : expr) : expr =
  if is_value e then e
  else e |> step |> eval_small
  
(** [eval e] is the [e ==> v] relation. *)
let rec eval (e : expr) : expr = match e with
  | Int _ | Bool _ -> e
  | Var _ -> runtime_error unbound_var_err
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
  | _ -> runtime_error "Operator and operand type mismatch"
  
(** [eval_if e1 e2 e3] is the [e] such that [if e1 then e2 else e3 ==> e]. *)
and eval_if e1 e2 e3 = match eval e1 with
  | Bool true -> eval e2
  | Bool false -> eval e3
  | Int _ -> failwith "Guard of if must have type bool"
  | _ -> runtime_error "precondition violated"

(** [string_of_val v] converts [v] to a string.
    Requires: [v] represents a value. *)
let string_of_val (e: expr) : string =
  match e with
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | _ -> runtime_error "precondition violated"  

(** [interp_small s] interprets [s] by parsing, type-checking,
    and evaluating it with the small-step model. *)
let interp_small (s : string) : expr =
  let e = parse s in
  ignore (typecheck e) ; eval_small e
    
(** [interp s] interprets [s] by lexing and parsing it,
    evaluating it, and covberting the result to a string. *)
let interp (s: string) : string =
  s |> parse |> typecheck |> eval |> string_of_val
  
(** [interp_big s] interprets [s] by parsing, type-checking,
  and evaluating it with the big-step model. *)
  let interp_big (s : string) : expr =
    let e = parse s in
    ignore (typecheck e); eval e