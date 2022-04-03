let extract lst i j =
  let rec inner_extract l n = function
    | [] -> l
    | h :: t ->
        if n >= i && n <= j then
          inner_extract (l @ [h]) (n+1) t
        else
          inner_extract l (n+1) t
  in inner_extract [] 0 lst;;

let print_lst lst =
  for i = 0 to (List.length lst) - 1 do
    print_string ((List.nth lst i) ^ " ")
  done;
  print_newline ();;

let extracted = extract ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;;
print_lst extracted;;
