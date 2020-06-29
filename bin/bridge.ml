open Bridge_lib

let repl = fun _ ->
  print_endline "Bridge Interpreter. 2020 Owain Lewis";
  while true do
    print_string "> ";
    let input = read_line () in
    if input = ":exit" then exit 0 else
      let program = Core.parse_string(input) in
      print_endline (Ast.to_string program);
  done

let run_file f =
  let program = Core.parse_file f in
  print_endline (Ast.to_string program)

let _ = match Array.length Sys.argv with
  | 1 -> repl ()
  | 2 -> run_file(Sys.argv.(1))
  | _ -> print_endline "Usage: bridge [script]";

