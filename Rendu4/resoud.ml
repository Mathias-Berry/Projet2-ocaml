open Type

exception Erreur

let affiche_type tab ti =  
  let rec aux k = match k with
    | Inte -> print_string "int"
    | Boole -> print_string "bool"
    | Unit -> print_string "Unit"
    | Tout -> print_string "'a"
    | Pasdef(i) -> begin match tab.(i) with
                    | Tout -> print_string "'"; print_int i
                    | x -> aux x
                   end
    | Tuples(a) -> print_string "( "; aux (List.hd a); let _ = List.map ( fun x -> print_string " * "; aux x) (List.tl a) in print_string " )"
    | Liste(t) -> print_string "( ";  aux t; print_string " ) list"
    | Fonc(t1, t2) -> print_string "( "; aux t1; print_string ") -> ("; aux t2; print_string " )"
    | Reff(t) -> print_string "( "; aux t; print_string " ) ref"
  in aux ti


let rec find tab i = match tab.(i) with
  | Pasdef(j) -> find tab j
  | Tout -> Pasdef(i)
  | x -> x



let resolution contr =
  let n = List.length contr in
  let encours = Array.make (100000) Tout in (* Ce tableau sert a stocker ce qu'on doit déjà savoir du type qui correspond à chaque numéro, avec la taile qu'il faut pour qu'on ait au moins assez de place ( on fait + 5 pour avoir de la marge pour pas s'embêter avec des problèmes de est ce que on commence à 1 ou 0 etc ... *)
	let bout = ref n in
	let rec aux l = match l with
	  | [] -> ()
	  | (i,b) :: q -> begin match b, find encours i with
					            | _, Pasdef(j) -> encours.(j) <- b; aux q
					            | Pasdef(s), t1 -> let t = find encours s in begin match t with
					                                                                        | Pasdef(j) -> encours.(j) <- t1; aux q
					                                                                        | _ -> incr bout; aux ( (!bout, t1) :: (!bout, t) :: q)
					            					                                                end
                      | _, Tout -> failwith "C'est la fete du slip !"
                      
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
					            
                      | Tout, _ -> aux q
					          
					          end
		in aux contr; encours 
