open OUnit2
open Maps

let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq Stdlib.compare lst1 in
  let uniq2 = List.sort_uniq Stdlib.compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

let binding_test name output input = 
  name >:: fun _ ->
    assert_equal output (AssocListMap.bindings input)
      ~cmp:cmp_set_like_lists

let lst1 = [(3110, "fun")]
let lst2 = [(3110, "fun"); (2110, "OO")]

let assoc_tests = let open AssocListMap in [
  binding_test "empty has no bindings" [] empty;
  binding_test "singleton list has 1 binding" lst1 (of_list lst1);
  binding_test "list with 2 bindings" lst2 (of_list lst2);
]

let suite = "map suite" >::: assoc_tests

let _ = run_test_tt_main suite