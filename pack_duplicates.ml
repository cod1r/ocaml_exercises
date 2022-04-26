let pack l =
  let rec rev r = match r with [] -> [] | hr :: tr -> rev tr @ [ hr ] in
  let rec acc lst = function
    | [] -> lst
    | h :: t -> (
        match lst with
        | [] -> acc [ [ h ] ] t
        | hh :: tt -> (
            match hh with
            | [] -> []
            | hhh :: thh ->
                if hhh = h then acc ((hh @ [ h ]) :: tt) t
                else acc ([ h ] :: lst) t))
  in
  rev (acc [] l)

let rec print_lst l =
  let rec print_element e =
    match e with
    | [] -> ()
    | h :: t ->
        print_string h;
        print_string " ";
        print_element t
  in
  match l with
  | [] -> print_newline ()
  | h :: t ->
      print_element h;
      print_newline ();
      print_lst t

let packed_alphabet = pack [ "a"; "a"; "b"; "b"; "a"; "a"; "a" ]

let packed_alphamore =
  pack
    [
      "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e";
    ]

let empty_l = pack [];;

print_lst packed_alphabet;;
print_lst packed_alphamore;;
print_lst empty_l
