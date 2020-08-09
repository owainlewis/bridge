open Bridge_lib

let report_error msg = print_endline ("Error: " ^ msg)

let repl = fun _ ->
  print_endline "Bridge Interpreter. 2020 Owain Lewis";
  let state = Interpreter.mk_state() in
  while true do
    print_string "> ";
    let input = read_line () in
    if input      = ":exit" then exit 0
    else if input = ":words" then print_endline "Words"
    else
      let program = Core.parse_string(input) in
      try
        let _ = Interpreter.interpret state program in ()
      with
      | Interpreter.State msg ->
        report_error msg
      | Interpreter.Unbound msg ->
        report_error msg
      | Interpreter.Type_error msg ->
        report_error msg
  done

let run_file f =
  let state = Interpreter.mk_state() in
  let program = Core.parse_file f in
  try
    let _ = Interpreter.interpret state program in ()
  with
  | Interpreter.State msg ->
    report_error msg
  | Interpreter.Unbound msg ->
    report_error msg

let _ = match Array.length Sys.argv with
  | 1 -> repl ()
  | 2 -> run_file(Sys.argv.(1))
  | _ -> print_endline "Usage: bridge [script]";

