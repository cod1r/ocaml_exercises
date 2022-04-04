let range s e =
  let rec acc l n =
    if s > e && n != e-1 then
      acc (l @ [n]) (n-1)
    else if s < e && n != e+1 then
      acc (l @ [n]) (n+1)
    else
      l
  in acc [] s;;

let print_lst lst =
  for i = 0 to (List.length lst) - 1 do
    print_string ((Int.to_string (List.nth lst i)) ^ " ")
  done;
  print_newline ();;

let ranged = range 4 9;;
let ranged_two = range 9 4;;
print_lst ranged;;
print_lst ranged_two;;
