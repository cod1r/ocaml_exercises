let rec reverse l = match l with [] -> [] | h :: t -> reverse t @ [ h ]

let rec print_lst lst =
  match lst with
  | [] -> ()
  | h :: t ->
      print_int h;
      print_newline ();
      print_lst t
;;

print_lst (reverse [ 1; 2; 3 ])
