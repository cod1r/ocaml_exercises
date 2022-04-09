let rec gcd x y =
  if x = 0 then
    y
  else
    gcd (y mod x) x;;

print_int (gcd 4 2);;
