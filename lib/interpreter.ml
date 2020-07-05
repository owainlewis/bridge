type state = { stack: Ast.expr Datastack.t }

module StringMap = Map.Make(struct type t = int let compare = Stdlib.compare end);;
let dictionary = StringMap.empty;;

let dot state = print_endline "OK"; state

let dictionary = [("dot", dot)]

let mk_state () = {
  stack = Datastack.create()
}

let interpret_one state = function
  | Ast.St_expr e ->
    Datastack.push e state.stack;
    state
  | _ -> state

let rec interpret state statements =
  match statements with
  | x::xs -> let new_state = interpret_one state x in interpret new_state xs
  | [] -> state
