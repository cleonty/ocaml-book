open Ast

(** [parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [Env] is module to help with environments, which 
    are maps that have strings as keys. *)
module Env = Map.Make(String)

(** [env] is the type of an environment, which maps
    a string to a value. *)
type env = value Env.t

(** [value] is the type of a lambda calculus value.
    In the environment model, that is a closure. *)
and value = 
  | Closure of string * expr * env
  | IntValue of int
  | BoolValue of bool

let unbound_var_err = "Unbound variable"

type scope_rule = Lexical | Dynamic
let scope = Lexical

(** [eval env e] is the [<env, e> ==> v] relation. *)
let rec eval (env : env) (e : expr) : value = match e with
  | Var x -> eval_var env x
  | App (e1, e2) -> eval_app env e1 e2
  | Fun (x, e) -> Closure (x, e, env)
  | Int x -> IntValue x
  | Bool v -> BoolValue v
  | Binop (bop, e1, e2) -> eval_bop env bop e1 e2
  | Let (x, e1, e2) -> eval_let env x e1 e2
  | If (e1, e2, e3) -> eval_if env e1 e2 e3
  | Fst (e) -> eval_fst env e
  | Snd (e) -> eval_snd env e
  | Pair (_, _) -> failwith "Pair is not first class value"
and eval_bop env bop e1 e2 = match bop, eval env e1, eval env e2 with
  | Add, IntValue a, IntValue b -> IntValue(a + b)
  | Mult, IntValue a, IntValue b -> IntValue(a * b)
  | Le, IntValue a, IntValue b -> BoolValue(a < b)
  | Leq, IntValue a, IntValue b -> BoolValue(a <= b)
  | Ge, IntValue a, IntValue b -> BoolValue(a > b)
  | Geq, IntValue a, IntValue b -> BoolValue(a >= b)
  | Equals, IntValue a, IntValue b -> BoolValue(a = b)
  | Equals, BoolValue a, BoolValue b -> BoolValue(a = b)
  | Add, _, _ -> failwith "Operator and operand type mismatch"
  | Mult, _, _ -> failwith "Operator and operand type mismatch"
  | Le, _, _ -> failwith "Operator and operand type mismatch"
  | Leq, _, _ -> failwith "Operator and operand type mismatch"
  | Ge, _, _ -> failwith "Operator and operand type mismatch"
  | Geq, _, _ -> failwith "Operator and operand type mismatch"
  | Equals, _, _ -> failwith "Operator and operand type mismatch"
and eval_let env x e1 e2 = 
  let v1 = eval env e1 in
  let env_for_let = Env.add x v1 env in
  eval env_for_let e2
and eval_if env e1 e2 e3 = match eval env e1 with
  | BoolValue true -> eval env e2
  | BoolValue false -> eval env e3
  | _ -> failwith "if condition must be boolean"
and eval_fst env e = match e with
  | Pair (e1, _) -> eval env e1
  | _ -> failwith "fst argument must be pair"
and eval_snd env e = match e with
  | Pair (_, e2) -> eval env e2
  | _ -> failwith "snd argument must be pair"
  

(** [eval_var env x] is the [v] such that [<env, x> ==> v]. *)
and eval_var env x = 
  try Env.find x env with Not_found -> failwith unbound_var_err

(** [eval_app env e1 e2] is the [v] such that [<env, e1 e2> ==> v]. *)
and eval_app env e1 e2 = 
  match eval env e1 with
  | Closure (x, e, defenv) -> begin
      let v2 = eval env e2 in
      let base_env_for_body = 
        match scope with
        | Lexical -> defenv
        | Dynamic -> env in
      let env_for_body = Env.add x v2 base_env_for_body in
      eval env_for_body e
    end
  | IntValue x -> IntValue x
  | BoolValue v -> BoolValue v

(** [interp s] interprets [s] by parsing
    and evaluating it with the big-step model,
    starting with the empty environment. *)
let interp (s : string) : value =
  s |> parse |> eval Env.empty
