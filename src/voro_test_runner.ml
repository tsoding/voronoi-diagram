open OUnit2
open VoroGeo

module Foo =
  struct
    let unity x = x
  end

let test1 test_ctxt = assert_equal "x" (Foo.unity "x")
let test2 test_ctxt = assert_equal 100 (Foo.unity 100)
let test_make_rect test_ctxt =
  let x, y, w, h = 0, 0, 10, 10 in
  let rect = VoroGeo.make_rect x y w h in
  assert_equal x (fst rect.position);
  assert_equal y (snd rect.position);
  assert_equal w (fst rect.size);
  assert_equal h (snd rect.size)

(* Name the test cases and group them together *)

let suite =
  "suite">:::
 ["test1">:: test1;
  "test2">:: test2;
  "test_make_rect" >:: test_make_rect]

let () = run_test_tt_main suite
