open OUnit2
open Bridge_lib

let test_parse_string _ =
  let input = "\"HELLO, WORLD!\"" in
  let expected = Ast.St_expr (Ast.Expr_string "HELLO, WORLD!") in
  assert_equal (Core.parse_string input) [expected]

let test_parse_int _ =
  let input = "1" in
  let expected = Ast.St_expr (Ast.Expr_int 1) in
  assert_equal (Core.parse_string input) [expected]

let suite =
  "TestLexer" >::: [
      "test_parse_string" >:: test_parse_string;
      "test_parse_int" >:: test_parse_int;
  ]

let () =
  run_test_tt_main suite
