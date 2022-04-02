type 'a node =
  | One of 'a
  | Many of 'a node list;;

let rec flatten l =
  match l with
  | [] -> []
  | h :: t ->
      match h with
      | One h -> h :: flatten t
      | Many h ->
          flatten h @ flatten t;;

let rec print_lst l =
  match l with
  | [] -> ()
  | h :: t ->
      print_int h;
      print_string " ";
      print_lst t;;

print_lst (flatten [One 1; Many [One 1;One 2; One 3]]);;
print_newline ();;
print_lst (flatten [Many [One 1;One 2;One 3]; Many [One 1;One 2;One 3]]);;
print_newline ();;
