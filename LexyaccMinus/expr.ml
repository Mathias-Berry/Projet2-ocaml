(* un type pour des expressions arithmétiques simples *)
type expr =
    Const of int
  | Add of expr*expr
  | Mul of expr*expr
  | Div of expr*expr
  | Min of expr*expr
  | Letin of expr*expr*expr
  | Variable of string
  | Ifte of expr*expr*expr
  | Lt of expr*expr
  | Le of expr*expr
  | Gt of expr*expr
  | Ge of expr*expr
  | Eg of expr*expr
  | And of expr*expr 
  | Or of expr*expr
  | Non of expr
  | Print of expr
  | Foction of expr*expr
  | Appli of expr*expr



(* fonction d'affichage *)
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
  | Add(e1,e2) -> aff_aux "Add(" e1 e2
  | Mul(e1,e2) -> aff_aux "Mul(" e1 e2
  | Div(e1,e2) -> aff_aux "Div(" e1 e2
  | Min(e1,e2) -> aff_aux "Min(" e1 e2
  | Variable s -> print_string s
  | Letin (a, b, c) -> (print_string "let "; affiche_expr a; print_string " = "; affiche_expr b; print_string " in "; affiche_expr c)
  | Ifte (a,b,c) -> (print_string "if "; affiche_expr a ; print_string " then "; affiche_expr b; print_string " else "; affiche_expr c)
  | Lt (a,b) ->(affiche_expr a; print_string " < "; affiche_expr b) 
  | Le (a,b) ->(affiche_expr a; print_string " <= "; affiche_expr b)
  | Gt (a,b) ->(affiche_expr a; print_string " > "; affiche_expr b)
  | Ge (a,b) ->(affiche_expr a; print_string " >= "; affiche_expr b)
  | Eg (a,b) ->(affiche_expr a; print_string " = "; affiche_expr b)
  | And (a,b) ->(affiche_expr a; print_string " && "; affiche_expr b)
  | Or (a,b) ->(affiche_expr a; print_string " || "; affiche_expr b)
  | Non (a) ->(print_string "Not "; affiche_expr a)
  | Print (a) ->(print_string "Print "; affiche_expr a)
  | Foction (a,b) ->(print_string f"fun"; affiche_expr a; print_string "->"; affiche_expr b)
  | Appli (a,b) ->(affiche_expr a; print_string " "; affiche_expr b)
(* sémantique opérationnelle à grands pas *)
