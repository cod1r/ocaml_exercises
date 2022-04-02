let rec print_pair p =
  match p with
  (a, b) -> print_int a; print_newline (); print_int b; print_newline ();;
let rec last_two l =
  match l with
  | [] -> None
  | [a] -> None
  | [i; j] -> Some (i, j)
  | h :: t -> last_two l;;

(*
  To print out the tuple we would need to deconstruct it, using pattern matching
*)
print_pair (Option.get (last_two [1;2]));;
