let insert_at e p lst =
  let rec acc l n = function
    | [] -> l
    | h :: t ->
        if n = p then
          acc (l @ [h; e]) (n+1) t
        else
          acc (l @ [h]) (n+1) t
  in acc [] 1 lst;;

let print_lst lst =
  for i = 0 to (List.length lst) - 1 do
    print_string ((List.nth lst i) ^ " ")
  done;
  print_newline ();;

let inserted = insert_at "alfa" 1 ["a"; "b"; "c"; "d"];;
let inserted_two = insert_at "alfa" 3 ["a"; "b"; "c"; "d"];;
let inserted_three = insert_at "alfa" 4 ["a"; "b"; "c"; "d"];;
print_lst inserted;;
print_lst inserted_two;;
print_lst inserted_three;;
