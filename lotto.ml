let lotto_select n m =
  let rec gen nth =
    match nth with
    | 1 -> [ Random.int m ]
    | _ -> [ Random.int m ] @ gen (nth - 1)
  in
  gen n

let print_lst lst =
  for i = 0 to List.length lst - 1 do
    print_string (Int.to_string (List.nth lst i) ^ " ")
  done;
  print_newline ()

let generated = lotto_select 6 49
let generated_two = lotto_select 5 5
let generated_three = lotto_select 10 10;;

print_lst generated;;
print_lst generated_two;;
print_lst generated_three
