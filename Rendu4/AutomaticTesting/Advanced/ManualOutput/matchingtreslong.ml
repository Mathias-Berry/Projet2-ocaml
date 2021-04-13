let f x = match x with
	| a, b -> 1
	| t :: (c, d, _) :: [a] -> 2
	| a, _ -> 3
	| a, b, c :: (t::2:: (_, _)::q) -> 4
	| t :: _ -> 5
	| 6 -> 6
	| t::q, t1::q1, _ -> 7
in prInt ( f (1, 2) ); prInt ( f ([1;(2, 3, [2]); 3])); prInt ( f ( 1, 2, [3; 4; 2; ([], [2;3])])); prInt ( f ( [1;2;3;4] ) ); prInt (f 6); prInt (f ([1; 2], [3, 4]))
