let f x = 2*x in prInt begin match f (3 + f 5) with | 0 -> 1 | k -> k end
