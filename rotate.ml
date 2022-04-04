let rotate l rn =
  let rec acc f s n = function
    | [] -> f @ s
    | h :: t ->
        if n > 0 then
          acc f (s @ [h]) (n-1) t
        else
          acc (f @ [h]) s n t
  in if rn > 0 then
    acc [] [] rn l
  else
    acc [] [] ((List.length l) + rn) l;;

let print_lst lst =
  for i = 0 to (List.length lst) - 1 do
    print_string ((List.nth lst i) ^ " ")
  done;
  print_newline ();;

let rotated = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
print_lst rotated;;
let rotated_two = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;
print_lst rotated_two;;
