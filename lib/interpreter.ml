(* This exception is raised when a user calls a word that is unbound.
 * A word is said to be `unbound` if there is no definition in the
 * internal or user defined dictionary.
*)
exception Unbound of string
(* This exception is raised when an invalid state is reached.
 * For example, a case where a user calls a word on a stack that does not
 * have the correct number of elements (An argument error)
*)
exception State of string

exception Type_error of string

type state = {
  stack: Ast.expr Datastack.t;
  dictionary: (string, Ast.expr list) Hashtbl.t
}

let mk_state () =
  {
    stack = Datastack.create();
    dictionary = Hashtbl.create 1024
  }

let push s e =
  Datastack.push s.stack e

let push_many s es =
  List.iter (push s) es

let set_word s k v = Hashtbl.add s.dictionary k v

let get_word s k = Hashtbl.find_opt s.dictionary k

let expr_list_to_string l =
  let elements = List.map Ast.expr_to_string l in
  let element_string = String.concat ", " (List.rev elements) in
  "[" ^ element_string ^ "]"

let print_user_dict s =
  Hashtbl.iter (fun k v -> print_endline (k ^ " = " ^ expr_list_to_string v)) s.dictionary

let debug state =
  let elements = Datastack.map Ast.expr_to_string state.stack in
  let element_string = String.concat ", " (List.rev elements) in
  print_endline("[" ^ element_string ^ "]");
  print_endline "Dictionary..";
  print_user_dict state;
  state

(* Primary operations that manipulate a stack or perform some kind of IO *)
let print state =
  let _ = debug state in
  match (Datastack.peek state.stack) with
  | Some(expr) -> print_endline (Ast.expr_to_string expr); state
  | None -> raise (State "Empty stack")
and dup state =
  match (Datastack.pop state.stack) with
  | Some e ->
    push_many state [e; e];
    state
  | None -> state
and swap state =
  match (Datastack.pop2 state.stack) with
  | Some((e1,e2)) ->
    push_many state [e1; e2];
    state
  | None -> state
and binop state f =
  match (Datastack.pop2 state.stack) with
  | Some((e1,e2)) ->
    push state (f e1 e2);
    state
  | None -> state
and plus x y =
  match (x,y) with
  | (Ast.Expr_int a, Ast.Expr_int b) -> Ast.Expr_int (a + b)
  | (Ast.Expr_float a, Ast.Expr_float b) -> Ast.Expr_float (a +. b)
  | _ -> raise (Type_error "Invalid types for `+` operation")
and minus x y =
  match (x,y) with
  | (Ast.Expr_int a, Ast.Expr_int b) -> Ast.Expr_int (a - b)
  | (Ast.Expr_float a, Ast.Expr_float b) -> Ast.Expr_float (a -. b)
  | _ -> raise (Type_error "Invalid types for `-` operation")
and multiply x y =
  match (x,y) with
  | (Ast.Expr_int a, Ast.Expr_int b) -> Ast.Expr_int (a * b)
  | (Ast.Expr_float a, Ast.Expr_float b) -> Ast.Expr_float (a *. b)
  | _ -> raise (Type_error "Invalid types for `*` operation")

(* A primary operation containing a name, stack modifier, doc string, signature and arity bound *)
type primary_op = {
  name: string;
  op: state -> state;
  signature: string;
  arity: int
}

let mk_op name f signature arity = {
  name = name; 
  op = f; 
  signature = signature; 
  arity = arity 
}

let op_dup =
  mk_op "dup" dup "[X] | dup => [X X]" 1
and op_swap =
  mk_op "swap" swap "[A B] | swap => [B A]" 2


(**
 * Interpret a single expression which will potentially modify
 * the state record.
 *
 * The return type of this function is a new state and potentially a list of expressions to be
 * evaluated. Expressions are returned by, for example, a user defined word
 *
 * E.g. let dup2 = dup dup; => interpret_one dup2 => (state, [dup, dup])
 *
 * The following example will cause an integer value to be pushed
 * into the runtime stack defined in the state record:
 *
 * Example: +> interpret_one state (Ast.St_expr (Ast.Expr_int 10))
*)
let interpret_one state = function
  | Ast.St_expr (Ast.Expr_id id) ->
    (match id with
     | "debug"    -> (debug state, [])
     | "print"    -> (print state, [])
     | "dup"      -> (dup state, [])
     | "swap"     -> (swap state, [])
     | "plus"     -> (binop state plus, [])
     | "minus"    -> (binop state minus, [])
     | "multiply" -> (binop state multiply, [])
     | _          ->
       (* Is this a user defined word? *)
       match (get_word state id) with
       (* TODO solve for recursive modifications *)
       | Some(exprs) -> (state, exprs)
       | None -> raise (Unbound ("Word `" ^ id ^ "` is not defined")))
  | Ast.St_expr e        -> Datastack.push state.stack e; (state, [])
  | Ast.St_assign (k, v) -> set_word state k v; (state, [])
  | Ast.St_module (_)    -> (state, [])

let rec interpret state statements =
  match statements with
  | x::xs ->
    let (new_state, exprs) = interpret_one state x in
    let statements = List.map (fun x -> Ast.St_expr x) exprs in
    interpret new_state (statements @ xs)
  | [] -> state
