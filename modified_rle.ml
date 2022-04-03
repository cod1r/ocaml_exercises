type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let encode l =
  let rec acc lst = function
    | [] -> lst
    | h :: t ->
        match lst with
        | [] -> acc [One h] t
        | hlst :: tlst ->
            match hlst with
            | One hlst -> 
                if hlst = h then
                  acc ((Many (2, h)) :: tlst) t
                else
                  acc ((One h) :: lst) t
            | Many (n, e) ->
                if e = h then
                  acc ((Many (n + 1, e)) :: tlst) t
                else
                  acc ((One h) :: lst) t
  in List.rev (acc [] l);;

let rec print_lst l =
  match l with
  | [] -> print_newline ()
  | h :: t ->
      match h with
      | One h -> 
          print_string (h ^ "\n"); 
          print_lst t
      | Many (n, e) -> 
          print_string (Int.to_string n ^ " " ^ e ^ "\n");
          print_lst t;;

let encoded = (encode ["a"; "a"; "b"]);;
let encoded_two = encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
print_lst encoded;;
print_lst encoded_two;;
