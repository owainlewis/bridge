module IStack = struct
  type 'a t = { mutable c : 'a list }

  exception Empty

  let create () = { c = [] }
  let clear s = s.c <- []
  let copy s = { c = s.c }
  let push x s = s.c <- x :: s.c

  let pop s =
    match s.c with
      x::xs -> s.c <- xs; Some(x)
    | [] -> None

  let unsafe_pop s =
    match s.c with
      x::xs-> s.c <- xs; x
    | []     -> raise Empty

  let peek s =
    match s.c with
      x::_ -> Some(x)
    | []     -> None

  let is_empty s = (s.c = [])
  let length s = List.length s.c
  let iter f s = List.iter f s.c
end



