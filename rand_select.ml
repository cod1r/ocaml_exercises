let rec rand_select lst n =
  match n with
  | 0 -> []
  | _ -> List.nth lst (Random.int (List.length lst)) :: rand_select lst (n-1)

let print_lst lst =
  for i = 0 to (List.length lst) - 1 do
    print_string ((List.nth lst i) ^ " ")
  done;
  print_newline ();;

let randomed = rand_select ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 7;;
print_lst randomed;;
