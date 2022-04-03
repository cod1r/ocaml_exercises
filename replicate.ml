let rec replicate lst n =
  let rec more n e =
    match n with
    | 0 -> []
    | _ -> (more (n-1) e) @ [e]
  in match lst with
  | [] -> []
  | h :: t ->
      (more n h) @ replicate t n;;

let replicated = replicate ["a"; "b"; "c"] 3;;
for i = 0 to (List.length replicated) - 1 do
  print_string (List.nth replicated i);
done;;
print_newline ();;
