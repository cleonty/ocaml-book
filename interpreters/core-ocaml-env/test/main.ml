open OUnit2
open Interp
open Ast
open Main

let rec code_part part = 
  match part with
  | (Closure (x, e, _)) -> Fun (x, e)
  | IntValue v -> Int (v)
  | BoolValue v -> Bool (v)
  | PairValue (v1, v2) -> Pair(code_part v1, code_part v2)
  | LeftValue (v) -> Left(code_part v)
  | RightValue (v) -> Right(code_part v)

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
  make "lets" "22" "let x = 0 in let x = 22 in x";
  make "if1" "22" "if true then 22 else 0";
  make "true" "true" "true";
  make "leq" "true" "1<=1";
  make "if2" "22" "if 1+2 <= 3+4 then 22 else 0";
  make "if3" "22" "if 1+2 <= 3*4 then let x = 22 in x else 0";
  make "letif" "22" "let x = 1+2 <= 3*4 in if x then 22 else 0";
  make "fst" "22" "fst (22, 23)";
  make "snd" "23" "snd (22, 23)";
  make "pair" "(1, 2)" "(1, 2)";
  make "Left and Right" "Left (Right 1)" "Left (Right 1)";
  make "match Left simple" "1" "match Left 2 with Left x -> 1 | Right x -> 2";
  make "match Left" "3" "match Left 2 with Left x -> x + 1 | Right x -> x - 1";
]

let _ = run_test_tt_main ("suite" >::: tests) 
