exception Unbound of string

type state = {
  stack: Ast.expr Datastack.t;
  dictionary: (string, (state -> state)) Hashtbl.t
}

let dot state =
  let parts = String.concat ", " (Datastack.map Ast.expr_to_string state.stack) in
  print_endline("[" ^ parts ^ "]");
  state

let mk_state () =
  (* Setup the hash table. Not maintainable so refactor *)
  let dict = Hashtbl.create 100
  and names = [("dot", dot)] in
  List.iter (fun (k,v) -> Hashtbl.add dict k v) names;
  {
    stack = Datastack.create();
    dictionary = dict
  }

let interpret_one state = function
  | Ast.St_expr (Ast.Expr_id id) ->
    (match (Hashtbl.find_opt state.dictionary id) with
     | Some(fn) ->
       print_endline "Found f";
       fn state (* Apply function to state *)
     | None -> raise (Unbound ("Word `" ^ id ^ "` is not defined")))
  | Ast.St_expr e -> Datastack.push e state.stack; state
  | Ast.St_assign (_, _) -> print_endline "Assign statement"; state
  | Ast.St_module (_) -> print_endline "Module statement"; state

let rec interpret state statements =
  match statements with
  | x::xs -> let new_state = interpret_one state x in interpret new_state xs
  | [] -> state
