let source = ref false
let debug = ref false

type arithop = 
  Add | Mul |Div |Min


type boolop1 =
   Eg | Ge | Gt | Le | Lt |Ne

type boolop2 =
   Or | And

type motif =
  | Varlistm of (string list)
  | Tuplem of (motif list)
  | Consm of motif*motif
  | Videm

type expr =
  | Const of int
  | Arithop of arithop*expr*expr
  | Letin of motif*expr*expr
  | Variable of string
  | Ifte of expr*expr*expr
  | Boolop1 of boolop1*expr*expr
  | Boolop2 of boolop2*expr*expr
  | Non of expr
  | Print
  | Fonction of string*expr
  | Appli of expr*expr
  | Letrec of string*expr*expr
  | Ref
  | Changeref of expr*expr
  | Valeurref of expr
  | Unite
  | Tuple of (expr list)
  | Listvide
  | Cons of expr*expr





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


let rec affiche_motif e = match e with
  | Varm (s) -> print_string s
  | Tuplem(l) -> print_string "("; affiche_listm l
  | Videm -> print_string "[]"
  | Consm(a, b) -> affiche_motif a; print_string ";;"; affiche_motif b

and affiche_listm l =
    match l with 
      |t::[]-> affiche_motif t;print_string ")"
      |t::q -> affiche_motif t; print_string","; affiche_listm q
      |_-> failwith "pas possible"

let rec affiche_expr e =
  match e with
  | Const k -> print_int k
  | Arithop (op,a,b) -> print_string "("; affiche_expr a; affichearithop op ; affiche_expr b; print_string ")"
  | Variable s -> print_string s
  | Letin (a, b, c) -> (print_string "let "; affiche_motif a; print_string " = "; affiche_expr b; print_string " in "; affiche_expr c)
  | Ifte (a,b,c) -> (print_string "if "; affiche_expr a ; print_string " then "; affiche_expr b; print_string " else "; affiche_expr c)
  | Boolop1 (op,a,b) -> print_string "("; affiche_expr a; afficheboolop1 op ; affiche_expr b; print_string ")"
  | Boolop2 (op,a,b) -> print_string "("; affiche_expr a; afficheboolop2 op ; affiche_expr b; print_string ")"
  | Non (a) ->(print_string "not("; affiche_expr a;print_string ")")
  | Print ->(print_string "prInt ")
  | Fonction (a,b) ->(print_string "(fun "; print_string a; print_string "->"; affiche_expr b; print_string ")")
  | Appli (a,b) ->(print_string "(";affiche_expr a; print_string "("; affiche_expr b; print_string ")) ")
  | Letrec (a, b, c) -> (print_string "let rec "; print_string a; print_string " = "; affiche_expr b; print_string " in "; affiche_expr c)
  | Ref->( print_string "ref " )
  | Changeref (a, b) -> (print_string "("; affiche_expr a; print_string ") := "; print_string "("; affiche_expr b; print_string ")" )
  | Valeurref (a) -> (print_string "!("; affiche_expr a; print_string ")")
  | Unite -> print_string "()"
  | Tuple l -> print_string "("; affiche_list l
  |_ -> failwith "ok"

and affiche_list l =
    match l with 
      |t::[]-> affiche_expr t;print_string ")"
      |t::q -> affiche_expr t; print_string","; affiche_list q
      |_-> failwith "pas possible"



let rec expr2motif e =
  match e with
    | Variable(v) -> Varm(v)
    | Listvide -> Videm
    | Cons(e1,e2)-> Consm(expr2motif(e1),expr2motif(e2))
    | Tuple(l)-> Tuplem(List.map expr2motif l)
    | _ -> failwith"ceci ne peut pas être un motif"








let rec letin m e =
  match m with
    | Varlistm l -> List.hd l, List.fold_right (fun x expr -> Fonction(x, expr)) (List.tl l) e
    | Videm -> 
    | Consm(t,q) -> 