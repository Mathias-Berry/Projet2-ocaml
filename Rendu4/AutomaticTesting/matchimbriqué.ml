let f x = match x with
	| [] -> 0
	| t::[] -> match t with | 0 -> 2 | _ -> 1
	| t::_ ->  3
in prInt (f []); prInt (f ([0])); prInt (f ([1])); prInt (f ([2; 3]))
