type bop =
  | Add
  | Mult
  | Leq

  (** The type represents SimPL types. *)
type typ =
  | TInt
  | TBool

type expr =
  | Var of string
  | Int of int
  | Bool of bool
  | Binop of bop * expr *expr
  | Let of string * typ * expr * expr
  | If of expr * expr * expr