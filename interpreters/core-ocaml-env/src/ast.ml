(** The type for the Binary operator nodes in AST  *)
type bop =
  | Add
  | Sub
  | Mult
  | Div
  | Leq
  | Le
  | Geq
  | Ge
  | Equals

(** The type for the Unary operator nodes in AST  *)
type unop = 
  | Uplus
  | Uminus

(** The type of the abstract syntax tree (AST). *)
type expr =
  | Var of string
  | App of expr * expr
  | Fun of string * expr
  | Int of int
  | Bool of bool
  | Binop of bop * expr * expr
  | Unop of unop * expr
  | Let of string * expr * expr
  | If of expr * expr * expr
  | Pair of expr * expr
  | Fst of expr
  | Snd of expr
  | Left of expr
  | Right of expr
  | Match of expr * string * expr * string * expr (** (e, x1, e1, x2, e2) *)

