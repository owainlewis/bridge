type state = { stack: Ast.expr Datastack.t }

let dot state = print_endline "OK"; state

let dictionary = [("dot", dot)]

let mk_state () = {
  stack = Datastack.create()
}

let push expr s =
  Datastack.push expr s.stack;
  s

let rec interpreter state statements =
  match statements with
  | x::xs -> match x with
      Ast.St_expr expr -> state
      _ -> state
  | [] -> state
