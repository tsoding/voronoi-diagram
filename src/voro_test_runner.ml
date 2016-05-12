open OUnit2

module Foo =
  struct
    let unity x = x
  end

let test1 test_ctxt = assert_equal "x" (Foo.unity "x")
let test2 test_ctxt = assert_equal 100 (Foo.unity 100)

(* Name the test cases and group them together *)

let suite =
  "suite">:::
 ["test1">:: test1;
  "test2">:: test2]

let () = run_test_tt_main suite
