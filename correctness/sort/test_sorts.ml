open OUnit2
open Sorts

let make func expected_output input _ =
  assert_equal expected_output (func input)

let ins_sort_tests =
  [
    (* first run tests with just this one test uncommented *)
    "ins sort empty list" >:: make ins_sort [] [];
    (* next uncomment just the next test *)
    "ins sort single-element list" >:: make ins_sort [1] [1];

    (* now uncomment the rest of the tests *)
      "ins sort two-element sorted list" >:: make ins_sort [1;2] [1;2];
      "ins sort two-element unsorted list" >:: make ins_sort [1;2] [2;1];
      "ins sort three-element unsorted list" >:: make ins_sort [1;2;3] [3;2;1];

    (* now try adding more glass box tests here *)
    "merge sort empty list" >:: make merge_sort [] [];
    "merge sort single-element list" >:: make merge_sort [1] [1];
    "merge sort two-element sorted list" >:: make merge_sort [1;2] [1;2];
    "merge sort two-element unsorted list" >:: make merge_sort [1;2] [2;1];
    "merge sort three-element unsorted list" >:: make merge_sort [1;2;3] [3;2;1];
    "merge sort four-element unsorted list" >:: make merge_sort [1;2;3;4] [4;2;3;1];
  ]

let suite = "test suite for sorts" >::: ins_sort_tests

let _ = run_test_tt_main suite
