let rand_select lst n =
  let rec acc lst n one two three = function
    | [] -> lst
    | h :: t ->
        if n = one || n = two || n = three then
          acc (lst @ [h]) (n+1) one two three t
        else
          acc lst (n+1) one two three t
  in acc [] 0 (Random.int (List.length lst)) 
              (Random.int (List.length lst)) 
              (Random.int (List.length lst)) lst;;

let print_lst lst =
  for i = 0 to (List.length lst) - 1 do
    print_string ((List.nth lst i) ^ " ")
  done;
  print_newline ();;

let randomed = rand_select ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
print_lst randomed;;
