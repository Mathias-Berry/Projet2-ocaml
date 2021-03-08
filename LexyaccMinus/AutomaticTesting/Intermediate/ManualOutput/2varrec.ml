let rec f x y = if y = 0 then 1 else (x * (f x (y-1))) in prInt (f 5 2)
