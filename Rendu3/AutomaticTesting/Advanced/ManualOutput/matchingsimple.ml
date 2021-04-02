let f x = match x with
	| a, 1 -> a
	| a, b -> b
	| _, _, (a,b)::q -> a + b
in prInt (f (1, 2)); prInt (f (3, 1)); prInt (f ([], [1;2], [5,5; 3]))
