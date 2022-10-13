(** The type for the Binary operator nodes in AST  *)
type bop =
  | Add
  | Mult

(** The type of the abstract syntax tree (AST). *)
type expr =
  | Var of string
  | App of expr * expr
  | Fun of string * expr
  | Int of int
  | Bool of bool
  | Binop of bop * expr * expr

