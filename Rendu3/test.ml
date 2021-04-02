let f x = raise (E x+1) in try f 5 with E 0 -> prInt 0 | E k -> prInt (k-1)
