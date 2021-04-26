let tab = Array.make 1000 (0, None);;

let recupsome x =
	match x with 
	|(_,Some k)-> k
	|(_,None)-> failwith"C'est une None";;

let rec find x = if snd(tab.(x)) != None then find (tab.(x)) else fst(tab.(x));;
let union k n = t.(k)<- (0,Some n);;

if (find 0) = Tout then print_int 1 else print_int 0;;

