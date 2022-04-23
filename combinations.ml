let extract g l =
  let rec pass_through arr n lst acc =
    match lst with
    | [] -> acc
    | h :: t ->
        arr.(n) <- h;
        if n < g - 1 then
          pass_through (Array.make g "") 0 t [] @ pass_through arr (n + 1) t acc
        else pass_through arr n t (acc @ [ Array.copy arr ])
  in
  pass_through (Array.make g "") 0 l []

let group = 1
let test = extract group [ "a"; "b"; "c"; "d"; "e" ]

let rec loop lst =
  match lst with
  | [] -> print_newline ()
  | h :: t ->
      for i = 0 to group - 1 do
        print_string (h.(i) ^ " ")
      done;
      print_newline ();
      loop t

let _ = loop test

(*
This is the solution on the website. I think it is much cleaner as it doesn't
use arrays. I had the right idea but didn't know how to execute it in a 
recursive manner that only used Lists.

let rec extract k list =
    if k <= 0 then [[]]
    else match list with
         | [] -> []
         | h :: tl ->
            let with_h = List.map (fun l -> h :: l) (extract (k - 1) tl) in
            let without_h = extract k tl in
            with_h @ without_h;;
*)
