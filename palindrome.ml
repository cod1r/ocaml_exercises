(*
  the last value in a function is the return value.
  if "true" is evaluated last, then it is returned.
  Same for "false"
*)
let rec is_pal l =
  let rec rev lst = match lst with [] -> [] | h :: t -> rev t @ [ h ] in
  if rev l = l then "true" else "false"
;;

print_string (is_pal [ 1; 2 ]);;
print_newline ();;
print_string (is_pal [ 1; 2; 3 ]);;
print_newline ();;
print_string (is_pal [ 1; 2; 1 ]);;
print_newline ();;
print_string (is_pal [ 1; 2; 2; 1 ]);;
print_newline ()
