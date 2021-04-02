let rec longueur l = match l with
	| [] -> 0
	| t::q  -> 1 + longueur q
in prInt (longueur (2::3::5::6::1::2::[]))
