open OUnit2
open VoroGeo

module Foo =
  struct
    let unity x = x
  end

let test1 test_ctxt = assert_equal "x" (Foo.unity "x")
let test2 test_ctxt = assert_equal 100 (Foo.unity 100)

let assert_rects_equal (rect1: rect) (rect2: rect): unit =
  assert_equal (fst rect1.position) (fst rect2.position);
  assert_equal (snd rect1.position) (snd rect2.position);
  assert_equal (fst rect1.size) (fst rect2.size);
  assert_equal (snd rect1.size) (snd rect2.size)

let test_split_rect_vert test_ctxt =
  let input_rect = make_rect 0 0 10 5 in
  let (expected_rect1, expected_rect2) = (make_rect 0 0 3 5,
                                         make_rect 3 0 7 5) in
  let (actual_rect1, actual_rect2) = split_rect_vert (3, 2) input_rect in
  assert_rects_equal actual_rect1 expected_rect1;
  assert_rects_equal actual_rect2 expected_rect2


(* Name the test cases and group them together *)

let suite =
  "suite">:::
 ["test1">:: test1;
  "test2">:: test2;
  "test_split_rect_vert" >:: test_split_rect_vert]

let () = run_test_tt_main suite
