exception Unbound of string
exception State of string

type state = {
  stack: Ast.expr Datastack.t;
  dictionary: (string, Ast.expr list) Hashtbl.t
}

let mk_state () =
  {
    stack = Datastack.create();
    dictionary = Hashtbl.create 1024
  }

let debug state =
  let elements = Datastack.map Ast.expr_to_string state.stack in
  let element_string = String.concat ", " (List.rev elements) in
  print_endline("[" ^ element_string ^ "]");
  state

(* Prints the first element on the stack [] -> ()) *)
let print state =
  match (Datastack.pop state.stack) with
  | Some(expr) -> print_endline (Ast.expr_to_string expr); state
  | None -> state

let dup state =
  match (Datastack.pop state.stack) with
  | Some e ->
    Datastack.push state.stack e;
    Datastack.push state.stack e;
    state
  | None -> state

let swap state =
  match (Datastack.pop2 state.stack) with
  | Some((e1,e2)) ->
    Datastack.push state.stack e1;
    Datastack.push state.stack e2;
    state
  | None -> state

(**
 * Interpret a single expression which will potentially modify
 * the state record.
 *
 * The following example will cause an integer value to be pushed
 * into the runtime stack defined in the state record:
 *
 * Example: +> interpret_one state (Ast.St_expr (Ast.Expr_int 10))
*)
let interpret_one state = function
  | Ast.St_expr (Ast.Expr_id id) ->
    (match id with
     | "debug" -> debug state
     | "print" -> print state
     | "dup"   -> dup state
     | "swap"  -> swap state
     | _       ->
       (* Is this a user defined word? *)
       match (Hashtbl.find_opt state.dictionary id) with
       (* TODO solve for recursive modifications *)
       | Some(_) -> state
       | None -> raise (Unbound ("Word `" ^ id ^ "` is not defined")))
  | Ast.St_expr e        -> Datastack.push state.stack e; state
  | Ast.St_assign (k, v) -> Hashtbl.add state.dictionary k v; state
  | Ast.St_module (_)    -> print_endline "Module statement"; state

let rec interpret state statements =
  match statements with
  | x::xs -> let new_state = interpret_one state x in interpret new_state xs
  | [] -> state
