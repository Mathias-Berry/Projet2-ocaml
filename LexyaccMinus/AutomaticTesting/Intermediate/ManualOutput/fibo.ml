let rec f x = if x <1  then 1 else f (x-2) + f (x-1) in
prInt (f 4)
