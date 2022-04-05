let rec merge_sort lst =
  match lst with
  | [] -> []
  | [i] -> [i]
  | _ ->
  let rec split ll = function
    | [] -> ll
    | h :: t ->
        match ll with
        | [] -> split [[h];[]] t
        | hll :: tll ->
            match tll with
            | [] -> []
            | htll :: ttll ->
                if List.length hll < (List.length (h :: t))/2 then
                  split ((hll @ [h]) :: (htll :: ttll)) t
                else
                  split (hll :: [(htll @ [h])]) t
  in 
  let merge a =
    match a with
    | [] -> []
    | h :: (hh :: tt) ->
        let rec merge_sorted left right merged = 
          match left with
          | [] -> 
              begin
                match right with
                | [] -> merged
                | headr :: tailr ->
                    merge_sorted left tailr (merged @ [headr])
              end;
          | hleft :: tleft ->
              begin
                match right with
                | [] -> merge_sorted tleft right (merged @ [hleft])
                | hright :: tright ->
                    if hleft > hright then
                      merge_sorted left tright (merged @ [hright])
                    else if hleft < hright then
                      merge_sorted tleft right (merged @ [hleft])
                    else
                      merge_sorted tleft tright (merged @ [hleft;hright])
              end;
        in merge_sorted (merge_sort h) (merge_sort hh) []
    | _ -> []
  in merge (split [] lst);;

let print_lst lst =
  for i = 0 to (List.length lst) - 1 do
    print_string ((Int.to_string (List.nth lst i)) ^ " ")
  done;
  print_newline ();;

let num_list = merge_sort [5;4;3;2;1];;
let num_sorted = merge_sort [5;5;4;4;3;3;2;0];;
let num_sorted_three = merge_sort [];;
let num_sorted_four = merge_sort [1;1;1;1;1];;
print_lst num_list;;
print_lst num_sorted;;
print_lst num_sorted_three;;
print_lst num_sorted_four;;
