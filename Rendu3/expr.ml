let source = ref false
let debug = ref false

type arithop = 
  Add | Mul |Div |Min


type boolop1 =
   Eg | Ge | Gt | Le | Lt |Ne

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
  | Pv of expr*expr
  | Ref of expr
  | Changeref of expr*expr
  | Valeurref of expr
  | Unite





(* fonction d'affichage *)
let affichearithop = function
  | Add -> print_string "+"
  | Mul -> print_string "*"
  | Div -> print_string "/"
  | Min -> print_string "-"

let afficheboolop1 = function
  | Eg -> print_string "="
  | Lt -> print_string "<"
  | Le -> print_string "<="
  | Gt -> print_string ">"
  | Ge -> print_string ">="
  | Ne -> print_string "<>"

let afficheboolop2 = function
  | And -> print_string " && "
  | Or -> print_string " || "


let rec affiche_expr e =
  match e with
  | Const k -> print_int k
  | Arithop (op,a,b) -> print_string "("; affiche_expr a; affichearithop op ; affiche_expr b; print_string ")"
  | Variable s -> print_string s
  | Letin (a, b, c) -> (print_string "let "; print_string a; print_string " = "; affiche_expr b; print_string " in "; affiche_expr c)
  | Ifte (a,b,c) -> (print_string "if "; affiche_expr a ; print_string " then "; affiche_expr b; print_string " else "; affiche_expr c)
  | Boolop1 (op,a,b) -> print_string "("; affiche_expr a; afficheboolop1 op ; affiche_expr b; print_string ")"
  | Boolop2 (op,a,b) -> print_string "("; affiche_expr a; afficheboolop2 op ; affiche_expr b; print_string ")"
  | Non (a) ->(print_string "not("; affiche_expr a;print_string ")")
  | Print (a) ->(print_string "prInt("; affiche_expr a; print_string ")")
  | Fonction (a,b) ->(print_string "(fun "; print_string a; print_string "->"; affiche_expr b; print_string ")")
  | Appli (a,b) ->(print_string "(";affiche_expr a; print_string "("; affiche_expr b; print_string ")) ")
  | Letrec (a, b, c) -> (print_string "let rec "; print_string a; print_string " = "; affiche_expr b; print_string " in "; affiche_expr c)
  | Ref (a) ->( print_string "ref ("; affiche_expr a; print_string ")" )
  | Changeref (a, b) -> (print_string "("; affiche_expr a; print_string ") := "; print_string "("; affiche_expr b; print_string ")" )
  | Valeurref (a) -> (print_string "!("; affiche_expr a; print_string ")")
  | Unite -> print_string "()"


