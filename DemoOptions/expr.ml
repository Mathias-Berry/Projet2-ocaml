(* un type pour des expressions arithmétiques simples *)
type expr =
    Const of int
  | Add of expr*expr
  | Mul of expr*expr




(* fonction d'affichage dépendant de deux booléens, pour illustrer l'utilisation des options *)
let lettres_capitales = ref false
  
let rec affiche_expr e =
  let aff_aux s a b = 
      begin
	print_string s;
	affiche_expr a;
	print_string ", ";
	affiche_expr b;
	print_string ")"
      end
  in
  match e with
  | Const k -> print_int k
  | Add(e1,e2) -> aff_aux (if !lettres_capitales then "ADD(" else "Add(") e1 e2
  | Mul(e1,e2) -> aff_aux (if !lettres_capitales then "MUL(" else "Mul(") e1 e2

(* sémantique opérationnelle à grands pas *)
let verbose = ref false

let p_verb s = if !verbose then Format.printf "%s\n" s else ()
    
let rec eval = function
  | Const k -> k
  | Add(e1,e2) -> (p_verb "oula, une addition"; eval e1 + eval e2)
  | Mul(e1,e2) -> (p_verb "oula, une multiplication"; eval e1 * eval e2)

  
