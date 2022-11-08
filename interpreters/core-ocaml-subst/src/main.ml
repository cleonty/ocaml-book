open Ast

(** [parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [is_value e] is whether [e] is a value. *)
let is_value : expr -> bool = function
  | Var _ -> false 
  | App _ -> false
  | Fun _ -> true
  | Int _ -> true
  | Bool _ -> true
  | Binop (_, _, _) -> false
  | Let (_, _, _) -> false
  | If (_, _, _) -> false
  | Pair (_, _) -> true
  | Fst _  -> false
  | Snd _  -> false

(** [string_of_expr e] is string that represents [e]. *)
let rec string_of_expr : expr -> string = function
  | Var x -> x 
  | App (e1, e2) -> (string_of_expr e1) ^ " " ^  (string_of_expr e2)
  | Fun (x, e) -> "fun " ^ x ^ " -> " ^ string_of_expr e 
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Binop (bop, e1, e2) -> (string_of_expr e1) ^ " " ^ (string_of_bop bop) ^ " " ^ (string_of_expr e2)
  | Let (x, e1, e2) -> "let " ^ x ^ " = " ^ (string_of_expr e1) ^ " in " ^ (string_of_expr e2) 
  | If (e1, e2, e3) -> "if " ^ (string_of_expr e1) ^ " then " ^ (string_of_expr e2) ^ " else " ^ (string_of_expr e3)
  | Pair (e1, e2) -> "(" ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
  | Fst (e) -> "fst (" ^ (string_of_expr e) ^ ")"
  | Snd (e) -> "snd (" ^ (string_of_expr e) ^ ")"

(** [string_of_expr e] is string that represents [e]. *)
and string_of_bop : bop -> string = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Leq -> "<="
  | Le -> "<"
  | Geq -> ">="
  | Ge -> ">"
  | Equals -> "="

module VarSet = Set.Make(String)
let singleton = VarSet.singleton
let union = VarSet.union
let diff = VarSet.diff
let mem = VarSet.mem
let empty = VarSet.empty

(** [fv e] is a set-like list of the free variables of [e]. *)
let rec fv : expr -> VarSet.t = function
  | Var x -> singleton x
  | App (e1, e2) -> union (fv e1) (fv e2)
  | Fun (x, e) -> diff (fv e) (singleton x)
  | Int _ -> empty
  | Bool _ -> empty
  | Binop (_, e1, e2) -> union (fv e1) (fv e2)
  | Let (x, e1, e2) -> union (fv e1) (diff (fv e2) (singleton x))
  | If (e1, e2, e3) -> union (fv e1) (union (fv e2) (fv e3)) 
  | Pair (e1, e2) -> union (fv e1) (fv e2)
  | Fst (e) -> (fv e)
  | Snd (e) -> (fv e)

(** [gensym ()] is a fresh variable name. *)
let gensym =
  let counter = ref 0 in
  fun () ->
    incr counter; "$x" ^ string_of_int !counter

(** [replace e y x] is [e] with the name [x] replaced
    by the name [y] anywhere it occurs. *)
let rec replace e y x = match e with
  | Var z -> if z = x then Var y else e
  | App (e1, e2) -> App (replace e1 y x, replace e2 y x)
  | Fun (z, e') -> Fun ((if z = x then y else z), replace e' y x)
  | Int i -> Int i
  | Bool b -> Bool b
  | Binop (bop, e1, e2) -> Binop(bop, replace e1 y x, replace e2 y x)
  | Let (x, e1, e2) -> Let (y, replace e1 y x, replace e2 y x)
  | If (e1, e2, e3) -> If (replace e1 y x, replace e2 y x, replace e3 y x)
  | Pair (e1, e2) -> Pair (replace e1 y x, replace e2 y x)
  | Fst (e) -> Fst (replace e y x)
  | Snd (e) -> Snd (replace e y x)

(** [subst e v x] is [e] with [v] substituted for [x], that
    is, [e{v/x}]. *)
let rec subst e v x = match e with
  | Var y -> if x = y then v else e
  | App (e1, e2) -> App (subst e1 v x, subst e2 v x)
  | Fun (y, e') -> 
    if x = y then e
    else if not (mem y (fv v)) then Fun (y, subst e' v x)
    else 
      let fresh = gensym () in
      let new_body = replace e' y fresh in
      Fun (fresh, subst new_body v x)
  | Int i -> Int i
  | Bool b -> Bool b
  | Binop (bop, e1, e2) -> Binop(bop, subst e1 v x, subst e2 v x)
  | Let (y, e1, e2) -> 
    if x = y then Let (x, subst e1 v x, e2)
    else if not (mem y (fv v)) then Let (y, subst e1 v x, subst e2 v x)
    else
      let fresh = gensym () in
      let new_e1 = replace e1 y fresh in
      let new_e2 = replace e2 y fresh in
      Let (fresh, subst new_e1 v x, subst new_e2 v x)
  | If (e1, e2, e3) -> If (subst e1 v x, subst e2 v x, subst e3 v x)
  | Pair (e1, e2) -> Pair (subst e1 v x, subst e2 v x)
  | Fst (e) -> Fst (subst e v x)
  | Snd (e) -> Snd (subst e v x)

let unbound_var_err = "Unbound variable"
let apply_non_fn_err = "Cannot apply non-function"

type eval_strategy = CBV | CBN
let strategy = CBV

(** [eval e] is the [e ==> v] relation. *)
let rec eval (e : expr) : expr = match e with
  | Var _ -> failwith unbound_var_err
  | App (e1, e2) -> eval_app e1 e2
  | Fun _ -> e
  | Int i -> Int i
  | Bool b -> Bool b
  | Binop (bop, e1, e2) -> eval_bop bop e1 e2
  | Let (x, e1, e2) -> eval_let x e1 e2
  | If (e1, e2, e3) -> eval_if e1 e2 e3
  | Pair (e1, e2) -> Pair (eval e1, eval e2)
  | Fst (e) -> eval_fst e
  | Snd (e) -> eval_snd e

(** [eval_app e1 e2] is the [e] such that [e1 e2 ==> e]. *)
and eval_app e1 e2 = match eval e1 with
  | Fun (x, e) -> 
    let e2' = 
      match strategy with
      | CBV -> eval e2
      | CBN -> e2
    in subst e e2' x |> eval
  | _ -> failwith apply_non_fn_err

(** [eval_let x e1 e2] is the [e] such that [let x = e1 in e2 ==> e]. *)
and eval_let x e1 e2 = 
  let e1' = eval e1 in eval (subst e2 e1' x)

(** [eval_if e1 e2 e3] is the [e] such that [if e1 then e2 else e3 ==> e]. *)
and eval_if e1 e2 e3 = match eval e1 with
  | Bool (true) -> eval e2
  | Bool (false) -> eval e3
  | _ -> failwith "not boolean IF condition"

(** [eval_fst e] is the [e] such that [fst (e1, e2)  ==> e]. *)
and eval_fst e = match eval e with
  | Pair (e1, _) -> eval e1
  | _ -> failwith "operand of fst is not a pair" 

(** [eval_snd e] is the [e] such that [snd (e1, e2)  ==> e]. *)
and eval_snd e = match eval e with
  | Pair (_, e2) -> eval e2
  | _ -> failwith "operand of snd is not a pair" 

(** [eval_bop e1 e2] is the [e] such that [e1 bop e2 ==> e]. *)
and eval_bop bop e1 e2 = match bop, eval e1, eval e2 with
| Add, Int i1, Int i2 -> Int (i1 + i2)
| Sub, Int i1, Int i2 -> Int (i1 - i2)
| Mult, Int i1, Int i2 -> Int (i1 * i2)
| Div, Int i1, Int i2 -> Int (i1 / i2)
| Leq, Int i1, Int i2 -> Bool (i1 <= i2)
| Le, Int i1, Int i2 -> Bool (i1 < i2)
| Geq, Int i1, Int i2 -> Bool (i1 >= i2)
| Ge, Int i1, Int i2 -> Bool (i1 > i2)
| Equals, Int i1, Int i2 -> Bool (i1 == i2)
| Equals, Bool b1, Bool b2 -> Bool (b1 == b2)
| Add, _, _ -> failwith "operator and operands type mismatch"
| Sub, _, _ -> failwith "operator and operands type mismatch"
| Mult, _, _ -> failwith "operator and operands type mismatch"
| Div, _, _ -> failwith "operator and operands type mismatch"
| Leq, _, _ -> failwith "operator and operands type mismatch"
| Le, _, _ -> failwith "operator and operands type mismatch"
| Geq, _, _ -> failwith "operator and operands type mismatch"
| Ge, _, _ -> failwith "operator and operands type mismatch"
| Equals, _, _ -> failwith "operator and operands type mismatch"

(** [interp s] interprets [s] by parsing
    and evaluating it with the big-step model. *)
let interp (s : string) : expr =
  s |> parse |> eval
