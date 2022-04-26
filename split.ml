let split lst s =
  let rec acc lst n = function
    | [] -> lst
    | h :: t -> (
        match lst with
        | [] -> acc [ [ h ] ] (n - 1) t
        | hlst :: tlst -> (
            if n > 0 then acc [ hlst @ [ h ] ] (n - 1) t
            else
              match tlst with
              | [] -> acc (lst @ [ [ h ] ]) (n - 1) t
              | htlst :: ttlst ->
                  acc (hlst :: (htlst @ [ h ]) :: ttlst) (n - 1) t))
  in
  acc [] s lst

let print_lst lst =
  for i = 0 to List.length lst - 1 do
    for j = 0 to List.length (List.nth lst i) - 1 do
      print_string (List.nth (List.nth lst i) j)
    done;
    print_newline ()
  done

let splitted = split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
let splitted_two = split [ "a"; "b"; "c"; "d" ] 5;;

print_lst splitted;;
print_lst splitted_two
