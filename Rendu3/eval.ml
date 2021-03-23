open Expr


type envi = (string*value) list
and value = Int of int | Fun of envi*string*expr*(string option) | Bool of bool | Refv of int | Unitv | Printv | Reff | Tuplev of (value list ) | Consv of value*value | Vide

(* Ici Reff représente la fonction ref, quand Refv représente une référence, un pointeur, en lui-même, au même titre que Int représente un entier *)

let reference = Array.make 1000 Unitv

let index = ref 0

(*let print_value x =
  match x with
    |Int k -> print_int k
    |Bool b -> if b then print_string "true" else print_string "false"
    |Fun (e,x,f,r)-> print_string ("fun "^x^" -> "); affiche_expr f
    |Unitv -> print_string"()"
    |Refv -> print_string "ref "
    |Printv -> print_string "prInt "
*)


let rec recup e s = match e with
  | [] -> failwith "La variable n'est pas définie"
  | (a, b)::q when a <> s -> recup q s
  | (a, b)::q -> b

let recupsome = function
  |Some s -> s
  |_ -> failwith "c'est None"

let recupvar  e =
  match e with
    | Variable x-> x
    | _ -> failwith"e n'est pas une variable"

let recupfonc  e =
  match e with
    | Fonction (x,f)-> (x,f)
    | _ -> failwith"e n'est pas une fonction"

let recupfun  v =
  match v with
    | Fun (env,x,f,r)-> (env,x,f,r)
    | Printv -> ([], "x", Unite, Some "1") (* je mets Some "1" parce que je suis sur que ca ne peut pas arriver en fouine puisque les noms de varibles ne peuvent pas commencer par des chiffres *)
    | Reff -> ([], "x", Unite, Some "2")
    | _ -> failwith"v n'est pas une fonction"

let recupint v =
  match v with
    | Int k-> k
    | _ -> failwith"v n'est pas un entier"

let recupbool v =
  match v with
    | Bool b-> b
    | _ -> failwith"v n'est pas un booléen"

let recupref v =
  match v with
    | Refv b -> b
    | _ -> failwith "l'argument n'est pas une référence"

let recuptuple v =
  match v with
    | Tuplev(l) -> l
    | _ -> failwith "J'aurai du recevoir un tuple"

let recupcons v =
  match v with 
    | Consv(a, b) -> (a, b)
    | _ -> failwith "J'aurai du recevoir un cons."

let estfun e = 
  match e with
    |Fonction(_,_) -> true
    |_ -> false


let estappli e = 
  match e with
    |Appli(_,_) -> true
    |_ -> false


let arithop2fun = function
  | Add -> fun x y -> x + y
  | Min -> fun x y -> x - y
  | Mul -> fun x y -> x * y
  | Div -> fun x y -> x / y


let boolop12fun = function
  | Eg -> fun x y -> (x = y)
  | Ge -> fun x y -> (x >= y)
  | Gt -> fun x y -> (x > y)
  | Le -> fun x y -> (x <= y)
  | Lt -> fun x y -> (x < y)
  | Ne -> fun x y -> (x <> y)


let boolop22fun = function
  | Or -> fun x y -> x || y
  | And -> fun x y -> x && y

(* Cette fonction plusieurs dit si on a une expression qui contient plusieurs information, et sera donc utiliser dans un let in, pour déterminer si on a une structure du type let (a, b) =  ou let t::q = ( et même let t1::t2::q etc ... ) *)



let rec eval env = function
  | Const k -> Int k
  | Arithop(op,e1,e2) -> Int ((arithop2fun op) (recupint (eval env e2)) (recupint (eval env e1)))
  | Variable s -> recup env s
  | Ifte(e1, e2, e3) -> if (recupbool (eval env e1)) then eval env e2 else eval env e3
  | Boolop1 (op, e1, e2) -> Bool ((boolop12fun op) (recupint (eval env e1)) (recupint (eval env e2)))
  | Boolop2 (op, e1, e2) -> Bool ((boolop22fun op) (recupbool (eval env e1)) (recupbool (eval env e2)))
  | Non(e) ->Bool (not (recupbool (eval env e)))
  | Fonction(x,e) ->Fun (env,x,e,None)
  | Letin(s, b, c) -> eval ( eval_affectation env s (eval env b) ) c
  | Letrec(s, b, c) -> let (x,f) = recupfonc b in
                                eval ( (s,(Fun (env,x,f, Some s))):: env ) c
  | Print -> Printv
  | Appli (e1, e2) -> let v2 = eval env e2 in
                      let a = eval env e1 in
                      let (envi,x,f,r)= recupfun a in
                      if r = None then
                        begin
                          eval ((x,v2)::envi) f
                        end
                      else
                        begin
                        match r with
                          | Some "1" -> begin print_int (recupint v2); print_newline (); v2 end
                          | Some "2" -> begin incr index; reference.(!index) <- v2; Refv(!index) end
                          | Some s -> eval ((s,a)::(x,v2)::envi) f
                        end
  | Ref -> Reff
  | Valeurref(e1) -> let s = eval env e1 in reference.(recupref s)
  | Changeref(e1, e2) -> let s = eval env e1 in let a = eval env e2 in (reference.(recupref s) <- a; Unitv)
  | Unite -> Unitv
  | Tuple(l) -> Tuplev of (List.map (eval env) l)
  | Cons(a, b) -> Consv( (eval env a), (eval env b))

and

  eval_affectation env s b = match s with
    | Tuplem([]) -> env
    | Tuplem(t::q) -> let l = recuptuple b in eval_affectation (eval_affectation env q (List.tl l)) t (List.hd l)
    | Listevidem -> env
    | Consv(a, q) -> let (c, d) = recupcons b in eval_affectation (eval_affectation env q d) a c
    | _ -> (s, b) :: env



  (*if estfun e1 then
                        begin
                          let v2 = eval env e2 in
                          let (x,f) = recupfonc e1 in
                          eval ((x,v2)::env) f
                         end
                      else
                        begin
                          let a = ref (Int 0) in
                          if estappli e1 then 
                            begin
                              a := eval env e1
                            end
                          else
                          begin
                            let v = recupvar e1 in
                            a := (recup env v)
                          end;
                          let (envi,x,f,r) = recupfun (!a) in
                          let v2 = eval env e2 in
                          if r = None then
                            begin
                              eval ((x,v2)::envi) f
                            end
                          else
                            begin
                              let s = recupsome r in
                              eval ((s,!a)::(x,v2)::envi) f
                            end
                        end*)


