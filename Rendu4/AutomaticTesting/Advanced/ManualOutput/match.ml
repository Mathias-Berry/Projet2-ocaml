let rec f = let rec a x = if x = 0 then 1 else a (x-1) + f (x-1) in a in prInt (f 10)
