open OUnit2
open Interp
open Main

(** [make n s1 s2] makes an OUnit test named [n] that expects
    [s2] to evalute to [s1]. *)
let make n s1 s2 =
  n >:: (fun _ -> assert_equal (parse s1) (interp s2))

(** [make_unbound_err n s] makes an OUnit test named [n] that
    expects [s] to produce an unbound variable error. *)
let make_unbound_err n s =
  n >:: (fun _ -> assert_raises (Failure unbound_var_err) (fun () -> interp s))

let tests = [
  make "reduce correct"
    "fun y -> y"
    "(fun x -> x) (fun y -> y)";
  make "shadowing correct"
    "fun a -> fun b -> b"
    "(fun x -> fun x -> x) (fun a -> fun b -> a) (fun a -> fun b -> b)";
  make_unbound_err "capture avoiding correct"
    "((fun x -> (fun z -> x)) z) (fun x -> x)";
  make "integer" "1" "1";
  make "arithmetic" "6" "2 + 2 * 2";
  make "arithmetic2" "8" "1 + 2 * 2 + 3";
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
    "2+2*2";
  make "2 < 3"
    "true"
    "2 < 3";
  make "2 <= 3"
    "true"
    "2 <= 3";
  make "3 < 2"
    "false"
    "3 < 2";
  make "3 <= 2"
    "false"
    "3 <= 2";
  make "3 = 2"
    "false"
    "3 = 2";
  make "3 = 3"
    "true"
    "3 = 3";
  make "true = true"
    "true"
    "true = true";
  make "false = false"
    "true"
    "false = false";
  make "let" "1" "let x = 1 in x";
  (* make "lets" "22" "let x = 0 in let x = 22 in x"; *)
  (* make "if1" "22" "if true then 22 else 0";
  make "true" "true" "true";
  make "leq" "true" "1<=1";
  make "if2" "22" "if 1+2 <= 3+4 then 22 else 0";
  make "if3" "22" "if 1+2 <= 3*4 then let x = 22 in x else 0";
  make "subtract" "0" "(fun x -> x - 1) 1";
  make "div" "2" "(fun x -> x / 2) 4";
  make "letif" "22" "let x = 1+2 <= 3*4 in if x then 22 else 0"; *)
]

let _ = run_test_tt_main ("suite" >::: tests)
