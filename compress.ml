let rec compress l =
  match l with
  | [] -> []
  | h :: t -> (
      match t with
      | [] -> []
      | ha :: ta -> if ha = h then ha :: compress ta else h :: compress t)

let rec print_lst l =
  match l with
  | [] -> ()
  | h :: t ->
      print_int h;
      print_string " ";
      print_lst t
;;

print_lst (compress [ 1; 1; 2; 2; 3; 3 ]);;
print_newline ()
