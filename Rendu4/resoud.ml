open Type

exception Erreur

let rec boucle n encours s1 s2 = match encours.(s2) with
   | Pasdef(s) when s = s1 -> true
   | Pasdef(s) -> boucle n encours s1 s
   | _ -> false


(* Cette fonction sert à prevoir la taille du tableau supplémentaires que l'on va devoir avoir pour pouvoir générer de nouvelles contraintes *)
let rec compte l = match l with
  | [] -> 0
  | Tuples(a) :: q -> List.length a + compte (a @ q)
  | Liste(t1)::q -> 1 + compte (t1 :: q)
  | Fonc(t1, t2) ::q -> 2 + compte (t1 :: t2 :: q)
  | Reff(t1) :: q -> 1 + compte (t1 :: q)
  | t::q -> compte q

let resolution contr =
  let n = List.length contr in
  let encours = Array.make (10000) Tout in (* Ce tableau sert a stocker ce qu'on doit déjà savoir du type qui correspond à chaque numéro, avec la taile qu'il faut pour qu'on ait au moins assez de place ( on fait + 5 pour avoir de la marge pour pas s'embêter avec des problèmes de est ce que on commence à 1 ou 0 etc ... *)
	let bout = ref n in
	let rec aux l = match l with
	  | [] -> ()
	  | (i,b) :: q -> begin match b, encours.(i) with
					            | Pasdef(s1), Pasdef(s2) -> if boucle n encours s1 s2 then aux q else encours.(i) <- Pasdef(s1); aux q
					            | _, Pasdef(s) -> aux ( (s,b)::q)
					            | Pasdef(s), t1 -> aux ( (s,t1) :: q) 
					            
					            | _, Tout -> encours.(i) <- b
					            
					            | Inte, Inte -> aux q
					            | Inte, _ -> raise Erreur
					            
                      | Boole, Boole -> aux q
					            | Boole, _ -> raise Erreur
					                          
					            | Unit, Unit -> aux q
                      | Unit, _ -> raise Erreur
					                         
					            | Fonc(t1, t2), Fonc(t3, t4) -> incr bout; incr bout; aux ( (!bout - 1, t1) :: (!bout - 1, t3) :: (!bout, t2) :: (!bout, t4) :: q)
					            | Fonc(t1, t2), _ -> raise Erreur
					            
					            | Liste(t1), Liste(t2) -> incr bout; aux ( (!bout, t1) :: (!bout, t2) :: q )
					            | Liste(t1), _ -> raise Erreur
					            
					            | Tuples([]), Tuples([]) -> aux q
					            | Tuples(t1::q1), Tuples([]) -> raise Erreur
					            | Tuples([]), Tuples(t2::q2) -> raise Erreur
					            | Tuples(t1::q1), Tuples(t2::q2) -> incr bout; incr bout; aux ( (!bout-1, t1) :: (!bout-1, t2) :: (!bout, Tuples(q1)) :: (!bout, Tuples(q2)) :: q)
					            | Tuples(li), _ -> raise Erreur
					            
					            | Reff(t1), Reff(t2) -> incr bout; aux ( (!bout, t1) :: (!bout, t2) :: q)
					            | Reff(t1), _ -> raise Erreur
					            
                      | Tout, _ -> ()
					          
					          end
		in aux contr; encours 
