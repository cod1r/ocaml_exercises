type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let decode lst =
  let rec multiply n e =
    match n with
    | 0 -> []
    | _ -> (multiply (n-1) e) @ [e]
    in let rec inner_decode l = function
      | [] -> []
      | h :: t ->
          match h with
          | One h ->
              h :: (inner_decode l t)
          | Many (n, e) ->
              (multiply n e) @ (inner_decode l t)
        in inner_decode [] lst;;

let rec print_lst lst =
  match lst with
  | [] -> print_newline ()
  | h :: t ->
      print_string (h ^ " ");
      print_lst t;;

let decoded = decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;
print_lst decoded;;
