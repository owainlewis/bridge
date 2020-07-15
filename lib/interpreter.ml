type state = { stack: Ast.expr Datastack.t }

let dot state =
  let parts = String.concat ", " (Datastack.map Ast.expr_to_string state.stack) in
  print_endline("[" ^ parts ^ "]");
  state

let dictionary = [("dot", dot)]

let mk_state () = {
  stack = Datastack.create()
}

let interpret_one state = function
  | Ast.St_expr (Ast.Expr_id id) -> print_endline id; state
  | Ast.St_expr e -> Datastack.push e state.stack; state
  | _ -> print_endline "Not implemented"; state

let rec interpret state statements =
  match statements with
  | x::xs -> let new_state = interpret_one state x in interpret new_state xs
  | [] -> state
