open OUnit2
open Bridge_lib

let test_parse_numbers _ =
  let expected = [Ast.St_expr (Ast.Expr_int 10)] in
  let actual = Core.parse_string "10" in
  assert_equal expected actual

let suite =
  "TestLexer" >::: [
    "test_parse_numbers" >:: test_parse_numbers;
  ]

let () =
  run_test_tt_main suite