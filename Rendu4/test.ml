let a = 2::[3] in let rec longueur l = match l with 
	| [] -> 0 
	| t::q -> 1 + longueur q 
in prInt (longueur a)
