let rec fib n =
  if n = 0 then 0
  else if n = 1 then 1
  else (fib (n - 2)) + (fib (n - 1)) in
prInt (fib 3)
