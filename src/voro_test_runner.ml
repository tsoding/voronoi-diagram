open OUnit2
open VoroGeo
open VoroList
open VoroGraphics

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
  let input_pivot = (3, 2) in
  let input_rect = make_rect 0 0 10 5 in
  let (expected_rect1, expected_rect2) = (make_rect 0 0 3 5,
                                         make_rect 3 0 7 5) in
  let (actual_rect1, actual_rect2) = split_rect_vert input_pivot input_rect in
  assert_rects_equal actual_rect1 expected_rect1;
  assert_rects_equal actual_rect2 expected_rect2

let test_split_rect_hor test_ctxt =
  let input_pivot = (3, 2) in
  let input_rect = make_rect 0 0 10 5 in
  let (expected_rect1, expected_rect2) = (make_rect 0 0 10 2,
                                          make_rect 0 2 10 3) in
  let (actual_rect1, actual_rect2) = split_rect_hor input_pivot input_rect in
  assert_rects_equal actual_rect1 expected_rect1;
  assert_rects_equal actual_rect2 expected_rect2

let test_group test_ctxt =
  let input1 = [3; 4; 5; 6; 2; 3] in
  let expected1 = [[3; 4]; [5; 6]; [2; 3]] in
  let input2 = [3; 4; 5; 6; 2] in
  let expected2 = [[3; 4]; [5; 6]; [2]] in
  assert_equal expected1 @@ group 2 input1;
  assert_equal expected2 @@ group 2 input2

let test_unrgb test_ctxt =
  assert_equal (255, 0, 0) @@ unrgb Graphics.red;
  assert_equal (0, 255, 0) @@ unrgb Graphics.green;
  assert_equal (0, 0, 255) @@ unrgb Graphics.blue;
  assert_equal (34, 100, 240) @@ unrgb @@ Graphics.rgb 34 100 240

(* Name the test cases and group them together *)

let suite =
  "suite">:::
 ["test1">:: test1;
  "test2">:: test2;
  "test_split_rect_vert" >:: test_split_rect_vert;
  "test_split_rect_hor" >:: test_split_rect_hor;
  "test_group" >:: test_group;
  "test_unrgb" >:: test_unrgb]

let () = run_test_tt_main suite
