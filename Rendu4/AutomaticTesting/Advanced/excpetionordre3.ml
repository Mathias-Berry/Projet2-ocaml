let rec f x y = match x,y with
	| 0, i -> raise (E i)
	| i, 0 -> 3
	| _ -> prInt (f x (y-1)) + prInt (f (x-1) y)
in try f 10 10 with | E k -> prInt k
