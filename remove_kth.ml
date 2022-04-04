let remove_at n lst =
  let rec acc l nth = function
    | [] -> l
    | h :: t ->
        if nth = n then
          acc l (nth+1) t
        else
          acc (l @ [h]) (nth+1) t
  in acc [] 0 lst;;

let print_lst l = 
  for i = 0 to (List.length l) - 1 do
    print_string ((List.nth l i) ^ " " )
  done;
  print_newline ();;

let removed = remove_at 1 ["a"; "b"; "c"; "d"];;
print_lst removed;;

