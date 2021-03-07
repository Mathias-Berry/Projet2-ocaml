(* un type pour des expressions arithmétiques simples *)
type arithop = 
  Add | Mul |Div |Min


type boolop1 =
   Eg | Ge | Gt | Le | Lt 

type boolop2 =
   Or | And

type expr =
  | Const of int
  | Arithop of arithop*expr*expr
  | Letin of string*expr*expr
  | Variable of string
  | Ifte of expr*expr*expr
  | Boolop1 of boolop1*expr*expr
  | Boolop2 of boolop2*expr*expr
  | Non of expr
  | Print of expr
  | Fonction of string*expr
  | Appli of expr*expr
  | Letrec of string*expr*expr






(* fonction d'affichage *)
let affichearithop = function
  | Add -> print_string "Add("
  | Mul -> print_string "Mul("
  | Div -> print_string "Div("
  | Min -> print_string "Min("

let afficheboolop1 = function
  | Eg -> print_string "Egal("
  | Lt -> print_string "Lt("
  | Le -> print_string "Le("
  | Gt -> print_string "Gt("
  | Ge -> print_string "Ge("

let afficheboolop2 = function
  | And -> print_string "And("
  | Or -> print_string "Or("


let rec affiche_expr e =
  match e with
  | Const k -> print_int k
  | Arithop (op,a,b) -> affichearithop op ; affiche_expr a; print_string "; "; affiche_expr b; print_string ")"
  | Variable s -> print_string s
  | Letin (a, b, c) -> (print_string "let "; print_string a; print_string " = "; affiche_expr b; print_string " in "; affiche_expr c)
  | Ifte (a,b,c) -> (print_string "if "; affiche_expr a ; print_string " then "; affiche_expr b; print_string " else "; affiche_expr c)
  | Boolop1 (op,a,b) -> afficheboolop1 op ; affiche_expr a; print_string "; "; affiche_expr b; print_string ")"
  | Boolop2 (op,a,b) -> afficheboolop2 op ; affiche_expr a; print_string "; "; affiche_expr b; print_string ")"
  | Non (a) ->(print_string "Not("; affiche_expr a;print_string ")")
  | Print (a) ->(print_string "Print("; affiche_expr a; print_string ")")
  | Fonction (a,b) ->(print_string "(fun "; print_string a; print_string "->"; affiche_expr b; print_string ")")
  | Appli (a,b) ->(affiche_expr a; print_string "("; affiche_expr b; print_string ") ")
  | Letrec (a, b, c) -> (print_string "let rec "; print_string a; print_string " = "; affiche_expr b; print_string " in "; affiche_expr c)
(* sémantique opérationnelle à grands pas *)

