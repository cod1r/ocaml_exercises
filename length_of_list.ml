let rec length l =
  match l with
  | [] -> 0
  | h :: t -> 1 + length t;;

let rec length_tail_rec l =
  let rec aux n = function
  | [] -> n
  | h :: t -> aux (n + 1) t
  in aux 0 l;;

print_int (length_tail_rec [1;2;3;4;5]);;
