open OUnit2
open Interp
open Ast
open Main

let code_part part = 
  match part with
  | (Closure (x, e, _)) -> Fun (x, e)
  | IntValue v -> Int (v)
  | BoolValue v -> Bool (v)

(** [make n s1 s2] makes an OUnit test named [n] that expects
    [s2] to evalute to [s1]. *)
let make n s1 s2 =
  n >:: (fun _ -> assert_equal (parse s1) (s2 |> interp |> code_part))

(** [make_unbound_err n s] makes an OUnit test named [n] that
    expects [s] to produce an unbound variable error. *)
let make_unbound_err n s =
  n >:: (fun _ -> assert_raises (Failure unbound_var_err) (fun () -> interp s))

(** This test suite is imperfect in that it only checks the code
    part of closures, not the environment part, for correctness. *)
let tests = [
  make "reduce correct"
    "fun y -> y"
    "(fun x -> x) (fun y -> y)";
  make "scope correct" (* lexical scope *)
    "(fun b -> b)"
    (* this is the example from the notes, but with
       - [fun a -> a] in place of [0]
       - [fun b -> b] in place of [1],
       - [fun c -> c] in place of [2];
       and with the [let] expressions desugared to functions. *)
    "(fun x -> \
     (fun f -> \
     (fun x -> \
     f (fun a -> a)) \
     (fun c -> c)) \
     (fun y -> x)) \
     (fun b -> b)";
  make "integer"
    "22"
    "22";
  make "true"
    "true"
    "true";
  make "2+2=4"
    "4"
    "2+2";
  make "2*2=4"
    "4"
    "2*2";
  make "2+2*2=6"
    "6"
    "2+2*2"
]

let _ = run_test_tt_main ("suite" >::: tests)
