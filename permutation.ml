(*
  Generate random permutation
*)
Random.self_init ()

let rec find lst e =
  match lst with
  | [] -> false
  | h :: t -> if h = e then true else false || find t e

let print_lst lst =
  for i = 0 to List.length lst - 1 do
    print_string (List.nth lst i ^ " ")
  done;
  print_newline ()

let permutation lst =
  let rec perm l alr =
    let len = List.length l in
    let len_alr = List.length alr in
    if len_alr < len then
      let rand_elem = List.nth l (Random.int len) in
      if find alr rand_elem then perm l alr else perm l (rand_elem :: alr)
    else alr
  in
  perm lst []

let permutationed = permutation [ "a"; "b"; "c"; "d"; "e"; "f" ];;

print_lst permutationed
