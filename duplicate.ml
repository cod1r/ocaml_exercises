let rec duplicate lst =
  match lst with
  | [] -> []
  | h :: t -> 
      [h;h] @ duplicate t;;

let rec print_lst lst =
  match lst with
  | [] -> print_newline ()
  | h :: t ->
      print_string (h ^ " ");
      print_lst t;;

let duplicated = duplicate ["a"; "b"; "c"; "c"; "d"];;
print_lst duplicated;;
