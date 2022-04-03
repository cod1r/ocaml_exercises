let encode l =
  let rec acc lst = function
    | [] -> lst
    | h :: t ->
        match lst with
        | [] -> acc [(h, 1)] t
        | hlst :: tlst ->
            match hlst with
            | (e, n) ->
                if e = h then
                  acc ((e, n + 1) :: tlst) t
                else
                  acc ((h, 1) :: lst) t
  in List.rev (acc [] l);;

let rec print_lst l =
  match l with
  | [] -> ()
  | h :: t ->
      match h with
      | (i, j) ->
          print_string (i ^ " " ^ (Int.to_string j) ^ "\n");
          print_lst t;;

let encoded = (encode ["a";"a";"b"]);;
let encoded_two = encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
print_int (List.length encoded);;
print_newline ();;
print_lst encoded;;
print_newline ();;
print_lst encoded_two;;
