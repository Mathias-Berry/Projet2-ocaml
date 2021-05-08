open Type

exception Erreur

let indexindef = Array.make (100000) '-'
let indef = ref 97

let affiche_type tab ti =
  let rec aux k = match k with
    | Inte -> print_string "int"
    | Boole -> print_string "bool"
    | Unit -> print_string "Unit"
    | Tout -> print_string "'a" (* Ce cas n'est pas censé arriver *)
    | Pasdef(i) -> begin match tab.(i) with
                    | Tout -> print_string "'"; (if indexindef.(i) = '-' then (indexindef.(i) <- char_of_int !indef; incr indef)); print_char indexindef.(i)
                    | x -> aux x
                   end
    | Tuples(a) -> print_string "( "; aux (List.hd a); let _ = List.map ( fun x -> print_string " * "; aux x) (List.tl a) in print_string " )"
    | Liste(t) -> print_string "( "; aux t; print_string " ) list"
    | Fonc(t1, t2) -> print_string "( "; aux t1; print_string ") -> ("; aux t2; print_string " )"
    | Reff(t) -> print_string "( "; aux t; print_string " ) ref"
  in aux ti


(*Cette fonction sert à trouver le représentant en bout de chaîne d'une classe d'équivalence*)
let rec find tab i = match tab.(i) with
  | Pasdef(j) -> find tab j
  | Tout -> Pasdef(i) (*Si on tombe sur Tout, c'est que 1) on est en bout de chaîne et 2) on doit renvoyer que maintenant on pointe vers ça  *)
  | x -> x (*Si on ne tombe pas sur un Tout, donc sur un type mieux défini, autant le renvoyer directement, pas besoin de sur alllonger les chaines des classes d'équivalence *)



let resolution contr n = (*n représente le dernier numéro utiliser dans les contraintes, pour savoir à partir de où je dois recommencer*)
  let encours = Array.make (100000) Tout in (* Ce tableau sert a stocker ce qu'on doit déjà savoir du type qui correspond à chaque numéro, onmet beaucoup de place car on sait pas faire autrement *)
	let bout = ref (n + 2) in
	let rec aux l = match l with
	  | [] -> ()
	  | (i,b) :: q -> begin match b, find encours i with
	        | Pasdef(s), Pasdef(j) -> let t = find encours s in begin match t with (* On doit faire cette étape pour éviter les boucles*)
	                                    | Pasdef(u) when u = j -> aux q (*comme le find nous amène au bout de la chaine des représentations, à priori, vérifié si u = j suffit, car sinon il ne sont pas encore dans la même classe d'équivalence *)
	                                    | _ -> encours.(j) <- b; aux q (*Ici on pourrait faire plus de filtrage pour raccourcir les chaînes de l'union find mais au point ou on en est. *)
	                                  end
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
          | Unit, x -> raise Erreur
                       
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
