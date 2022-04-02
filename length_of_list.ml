let rec length l =
  match l with
  | [] -> 0
  | h :: t -> 1 + length t;;

(*
  function keyword takes in one argument and matches it 
  with patterns and if there is a match the resulting expression,
  that corresponds with the pattern is evaluated and the expression's
  value is made to be the function's value.

  fun keyword takes in n number of arguments and evaluated an expression based
  on those arguments. It does not do pattern matching.
*)
let rec length_tail_rec l =
  let rec aux n = function
  | [] -> n
  | h :: t -> aux (n + 1) t
  in aux 0 l;;

print_int (length_tail_rec [1;2;3;4;5]);;
