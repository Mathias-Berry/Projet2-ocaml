let source = ref false
let debug = ref false

type arithop = 
  Add | Mul |Div |Min


type boolop1 =
   Eg | Ge | Gt | Le | Lt |Ne

type boolop2 =
   Or | And

type motif =
  | Varm of string
  | Tuplem of (motif list)
  | Consm of motif*motif
  | Videm
  | Constm of int

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
  | Fonction of motif*expr
  | Appli of expr*expr
  | Letrec of string*expr*expr
  | Ref
  | Changeref of expr*expr
  | Valeurref of expr
  | Unite
  | Tuple of (expr list)
  | Listvide
  | Cons of expr*expr
  | Match of (expr*((motif*expr) list))
  | Raise of expr
  | Try of (expr*((motif*expr) list))
  | Fst
  | Snd





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
  | Varm (l) -> print_string l
  | Tuplem(l) -> print_string "("; affiche_listm l
  | Videm -> print_string "[]"
  | Consm(a, b) -> affiche_motif a; print_string "::"; affiche_motif b
  | Constm k -> print_int k

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
  | Fonction (a,b) ->(print_string "(fun "; affiche_motif a; print_string " -> "; affiche_expr b; print_string ")")
  | Appli (a,b) ->(affiche_expr a; print_string "("; affiche_expr b; print_string ") ")
  | Letrec (a, b, c) -> (print_string "let rec"; print_string  a; print_string " = "; affiche_expr b; print_string " in "; affiche_expr c)
  | Ref->( print_string "ref " )
  | Changeref (a, b) -> (print_string "("; affiche_expr a; print_string ") := "; print_string "("; affiche_expr b; print_string ")" )
  | Valeurref (a) -> (print_string "!("; affiche_expr a; print_string ")")
  | Unite -> print_string "()"
  | Tuple l -> print_string "("; affiche_list l
  | Listvide -> print_string "[]"
  | Cons(a, b) -> begin print_string "("; affiche_expr a; print_string ") :: ("; affiche_expr b; print_string ")" end
  | Match(a, b) -> begin print_string "match "; affiche_expr a; print_string " with "; print_newline (); let _ = List.map (fun x -> (print_string "| "; affiche_motif (fst x); print_string " -> "; affiche_expr (snd x); print_newline() ) ) b in () end 
  | Raise k -> print_string"raise (E ";affiche_expr k ; print_string")"
  | Try (a,b) -> begin print_string "try "; affiche_expr a; print_string " with "; print_newline (); let _ = List.map (fun x -> (print_string "| "; affiche_motif (fst x); print_string " -> "; affiche_expr (snd x); print_newline() ) ) b in () end 
  | Fst -> print_string"fst"
  | Snd -> print_string"snd"


and affiche_list l =
    match l with 
      |t::[]-> affiche_expr t;print_string ")"
      |t::q -> affiche_expr t; print_string","; affiche_list q
      |_-> failwith "pas possible"


let recupvar m =
  match m with
  | Varm x -> x
  | _ -> failwith"impossible"