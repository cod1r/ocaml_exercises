(* the below code returns the last element 
   wrapped in Ocaml's builtin 'a option type *)
let rec last l =
  match l with
  | [] -> None
  | [i] -> Some i
  | _ :: t -> 
      last t;;
(* We use Option.get to retrieve the value from the 'a option type *)
(* if we get None from Option.get, 
   an exception is thrown so we wrap
   it around with a try block and use 
   pattern match to return a unit type *)
try print_int (Option.get (last [1;2;3;4;5])) with _ -> ();;
print_newline ();;
