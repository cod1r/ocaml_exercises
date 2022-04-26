let drop lst nth =
  let rec acc n nn new_lst = function
    | [] -> new_lst
    | h :: t ->
        if nn * (n / nn) = n then acc (n + 1) nn new_lst t
        else acc (n + 1) nn (new_lst @ [ h ]) t
  in
  acc 1 nth [] lst

let print_lst l =
  for i = 0 to List.length l - 1 do
    print_string (List.nth l i ^ " ")
  done;
  print_newline ()

let dropped = drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3;;

print_lst dropped

let dropped_two =
  drop
    [ "a"; "a"; "a"; "a"; "a"; "a"; "a"; "a"; "a"; "a"; "a"; "a"; "a"; "a" ]
    2
;;

print_lst dropped_two
