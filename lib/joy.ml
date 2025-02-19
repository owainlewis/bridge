(* Joy Lang *)

(* Core stack operations *)
let dup state =
  match state.stack with
  | x :: rest -> Ok { state with stack = x :: x :: rest }
  | [] -> Error Stack_underflow

let swap state =
  match state.stack with
  | x :: y :: rest -> Ok { state with stack = y :: x :: rest }
  | _ -> Error Stack_underflow

let pop state =
  match state.stack with
  | _ :: rest -> Ok { state with stack = rest }
  | [] -> Error Stack_underflow

let roll state =
  match state.stack with
  | Int n :: rest ->
      let rec roll_n n xs =
        if n = 0 then xs
        else match xs with
          | x :: y :: ys -> roll_n (n-1) (y :: x :: ys)
          | _ -> xs
      in
      if List.length rest >= 2
      then Ok { state with stack = roll_n n rest }
      else Error Stack_underflow
  | _ -> Error (Type_error "roll requires an integer")

let rollup state =
  match state.stack with
  | x :: y :: z :: rest -> Ok { state with stack = y :: z :: x :: rest }
  | _ -> Error Stack_underflow

let rolldown state =
  match state.stack with
  | x :: y :: z :: rest -> Ok { state with stack = z :: x :: y :: rest }
  | _ -> Error Stack_underflow

(* Core quotation operations *)
let i state =
  match state.stack with
  | Quote prog :: rest ->
      eval_sequence prog { state with stack = rest }
  | _ -> Error (Type_error "i requires a quotation")

let dip state =
  match state.stack with
  | Quote prog :: x :: rest ->
      eval_sequence prog { state with stack = rest } >>= fun new_state ->
      Ok { new_state with stack = x :: new_state.stack }
  | _ -> Error (Type_error "dip requires a quotation and a value")

let infra state =
  match state.stack with
  | Quote prog :: Quote data :: rest ->
      let state' = { state with stack = data } in
      eval_sequence prog state' >>= fun new_state ->
      Ok { state with stack = Quote new_state.stack :: rest }
  | _ -> Error (Type_error "infra requires two quotations")

(* List operations *)
let first state =
  match state.stack with
  | List (x :: _) :: rest -> Ok { state with stack = x :: rest }
  | Quote (x :: _) :: rest -> Ok { state with stack = x :: rest }
  | _ -> Error (Type_error "first requires a non-empty aggregate")

let rest state =
  match state.stack with
  | List (_ :: xs) :: rest -> Ok { state with stack = List xs :: rest }
  | Quote (_ :: xs) :: rest -> Ok { state with stack = Quote xs :: rest }
  | _ -> Error (Type_error "rest requires a non-empty aggregate")

let cons state =
  match state.stack with
  | x :: List xs :: rest -> Ok { state with stack = List (x :: xs) :: rest }
  | x :: Quote xs :: rest -> Ok { state with stack = Quote (x :: xs) :: rest }
  | _ -> Error (Type_error "cons requires an element and an aggregate")

let concat state =
  match state.stack with
  | List ys :: List xs :: rest -> Ok { state with stack = List (xs @ ys) :: rest }
  | Quote ys :: Quote xs :: rest -> Ok { state with stack = Quote (xs @ ys) :: rest }
  | _ -> Error (Type_error "concat requires two aggregates of the same type")

let size state =
  match state.stack with
  | List xs :: rest -> Ok { state with stack = Int (List.length xs) :: rest }
  | Quote xs :: rest -> Ok { state with stack = Int (List.length xs) :: rest }
  | String s :: rest -> Ok { state with stack = Int (String.length s) :: rest }
  | _ -> Error (Type_error "size requires an aggregate")

(* Type predicates *)
let type_of state =
  match state.stack with
  | x :: rest ->
      let type_str = match x with
        | Int _ -> "integer"
        | Float _ -> "float"
        | Bool _ -> "boolean"
        | String _ -> "string"
        | Char _ -> "char"
        | Quote _ -> "quote"
        | List _ -> "list"
        | Symbol _ -> "symbol"
        | UserDef _ -> "user-defined"
      in
      Ok { state with stack = String type_str :: rest }
  | [] -> Error Stack_underflow

let small state =
  match state.stack with
  | x :: rest ->
      let is_small = match x with
        | Quote _ | List _ -> false
        | _ -> true
      in
      Ok { state with stack = Bool is_small :: rest }
  | [] -> Error Stack_underflow

(* Arithmetic *)
let numeric_op op state =
  match state.stack with
  | Int b :: Int a :: rest -> Ok { state with stack = Int (op a b) :: rest }
  | Float b :: Float a :: rest -> Ok { state with stack = Float (op a b) :: rest }
  | Float b :: Int a :: rest -> Ok { state with stack = Float (op (float_of_int a) b) :: rest }
  | Int b :: Float a :: rest -> Ok { state with stack = Float (op a (float_of_int b)) :: rest }
  | _ -> Error (Type_error "numeric operation requires two numbers")

let add = numeric_op (+)
let sub = numeric_op (-)
let mul = numeric_op ( * )
let div state =
  match state.stack with
  | Int 0 :: _ :: _ -> Error (Type_error "division by zero")
  | Float 0. :: _ :: _ -> Error (Type_error "division by zero")
  | x :: y :: rest -> numeric_op (/) state
  | _ -> Error Stack_underflow

(* Comparison *)
let compare_op op state =
  match state.stack with
  | Int b :: Int a :: rest -> Ok { state with stack = Bool (op a b) :: rest }
  | Float b :: Float a :: rest -> Ok { state with stack = Bool (op a b) :: rest }
  | Float b :: Int a :: rest -> Ok { state with stack = Bool (op (float_of_int a) b) :: rest }
  | Int b :: Float a :: rest -> Ok { state with stack = Bool (op a (float_of_int b)) :: rest }
  | _ -> Error (Type_error "comparison requires two numbers")

let lt = compare_op (<)
let gt = compare_op (>)
let eq state =
  match state.stack with
  | y :: x :: rest -> Ok { state with stack = Bool (x = y) :: rest }
  | _ -> Error Stack_underflow

(* Advanced recursion combinators *)
let rec linrec state =
  match state.stack with
  | Quote rec_case :: Quote comb :: Quote base :: Quote pred :: rest ->
      let rec loop s =
        eval_sequence pred s >>= fun pred_state ->
        match pred_state.stack with
        | Bool true :: rest_stack ->
            eval_sequence base { pred_state with stack = rest_stack }
        | Bool false :: rest_stack ->
            eval_sequence rec_case { pred_state with stack = rest_stack } >>= fun rec_state ->
            loop rec_state >>= fun result_state ->
            eval_sequence comb result_state
        | _ -> Error (Type_error "predicate must return boolean")
      in
      loop { state with stack = rest }
  | _ -> Error (Type_error "linrec requires four quotations")

let rec binrec state =
  match state.stack with
  | Quote comb :: Quote rec2 :: Quote rec1 :: Quote base :: Quote pred :: rest ->
      let rec loop s =
        eval_sequence pred s >>= fun pred_state ->
        match pred_state.stack with
        | Bool true :: rest_stack ->
            eval_sequence base { pred_state with stack = rest_stack }
        | Bool false :: rest_stack ->
            eval_sequence rec1 { pred_state with stack = rest_stack } >>= fun rec1_state ->
            loop rec1_state >>= fun result1_state ->
            eval_sequence rec2 result1_state >>= fun rec2_state ->
            loop rec2_state >>= fun result2_state ->
            eval_sequence comb result2_state
        | _ -> Error (Type_error "predicate must return boolean")
      in
      loop { state with stack = rest }
  | _ -> Error (Type_error "binrec requires five quotations")

(* I/O operations *)
let putchars state =
  match state.stack with
  | String s :: rest ->
      print_string s;
      flush stdout;
      Ok { state with stack = rest }
  | _ -> Error (Type_error "putchars requires a string")

let getchars state =
  try
    let line = read_line () in
    Ok { state with stack = String line :: state.stack }
  with
    End_of_file -> Error (Type_error "end of input")

(* System operations *)
let getenv state =
  match state.stack with
  | String var :: rest ->
      begin match Sys.getenv_opt var with
      | Some value -> Ok { state with stack = String value :: rest }
      | None -> Ok { state with stack = Quote [] :: rest }
      end
  | _ -> Error (Type_error "getenv requires a string")

let gc state =
  Gc.compact ();
  Ok state

(* Map/Filter/Fold *)
let map state =
  match state.stack with
  | Quote prog :: List data :: rest ->
      let rec map_aux = function
        | [] -> Ok []
        | x :: xs ->
            let state' = { state with stack = [x] } in
            eval_sequence prog state' >>= fun new_state ->
            match new_state.stack with
            | [result] ->
                map_aux xs >>= fun rest_results ->
                Ok (result :: rest_results)
            | _ -> Error (Type_error "map: program must leave exactly one result")
      in
      map_aux data >>= fun results ->
      Ok { state with stack = List results :: rest }
  | _ -> Error (Type_error "map requires a quotation and a list")

let filter state =
  match state.stack with
  | Quote pred :: List data :: rest ->
      let rec filter_aux = function
        | [] -> Ok []
        | x :: xs ->
            let state' = { state with stack = [x] } in
            eval_sequence pred state' >>= fun new_state ->
            match new_state.stack with
            | [Bool keep] ->
                filter_aux xs >>= fun rest_results ->
                Ok (if keep then x :: rest_results else rest_results)
            | _ -> Error (Type_error "filter: predicate must leave a boolean")
      in
      filter_aux data >>= fun results ->
      Ok { state with stack = List results :: rest }
  | _ -> Error (Type_error "filter requires a quotation and a list")

let fold state =
  match state.stack with
  | Quote prog :: List (x :: xs) :: rest ->
      let rec fold_aux acc = function
        | [] -> Ok acc
        | y :: ys ->
            let state' = { state with stack = y :: acc :: [] } in
            eval_sequence prog state' >>= fun new_state ->
            match new_state.stack with
            | [result] -> fold_aux result ys
            | _ -> Error (Type_error "fold: program must leave exactly one result")
      in
      fold_aux x xs >>= fun result ->
      Ok { state with stack = result :: rest }
  | _ -> Error (Type_error "fold requires a quotation and a non-empty list")

(* Register all native combinators *)
let native_combinators = StringMap.of_list [
  (* Stack operations *)
  ("dup", dup);
  ("swap", swap);
  ("pop", pop);
  ("roll", roll);
  ("rollup", rollup);
  ("rolldown", rolldown);

  (* Quote operations *)
  ("i", i);
  ("dip", dip);
  ("infra", infra);

  (* List operations *)
  ("first", first);
  ("rest", rest);
  ("cons", cons);
  ("concat", concat);
  ("size", size);

  (* Type predicates *)
  ("type", type_of);
  ("small", small);

  (* Arithmetic *)
  ("+", add);
  ("-", sub);
  ("*", mul);
  ("/", div);

  (* Comparison *)
  ("<", lt);
  (">", gt);
  ("=", eq);

  (* Recursion *)
  ("linrec", linrec);
  ("binrec", binrec);

  (* I/O *)
  ("putchars", putchars);
  ("getchars", getchars);

  (* System *)
  ("getenv", getenv);
  ("gc", gc);

  (* Higher-order functions *)
  ("map", map);
  ("filter", filter);
  ("fold", fold);
]
