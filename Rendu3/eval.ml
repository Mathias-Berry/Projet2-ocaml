open Expr


type envi = (string*value) list
and value = Int of int | Fun of envi*motif*expr*(string option) | Bool of bool | Refv of int | Unitv | Tuplev of (value list ) | Consv of value*value | Vide | Except of int


let reference = Array.make 1000 Unitv

let index = ref 0

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

let estexcept v = 
  match v with
    | Except(_) -> true
    | _ -> false

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

(* la fonction suivante sert à savoir si le motif que l'on a sous les yeux peut correspondre à une liste. Ainsi ca permet de tester si on veut nous entourlouper en faisant des trucs du genre *)
let testlistem = function
  | Consm (_, _) -> true
  | Videm -> true
  | Varm("_") -> true
  | _ -> false
  
(* Là on fait une fonction qui dit si une liste d'éléments contient deux fois le élément, ce qui nous servira pour le matching pour tester si on veut nous filouter en attribuant deux fois le même nom de variables, ce que Caml ne semble pas vraiment apprécier *)
let rec doublon l = match l with
  | [] -> false
  | t::q -> (List.mem t q) || (doublon q)
  
exception NOMATCH (* On aura besoin de cette exception quand on fera le matching, car on parcourera récursivement le matching, et si a un moment on voit que ca marche pas, au lieu de se passer un booléen pour se dire que ca va pas, on soulève une exception pour dire hop hop hop, on passe au truc suivant *)

let rec eval env = function
  | Const k -> Int k
  | Arithop(op,e1,e2) -> let inter2 = eval env e2 in if estexcept inter2 then inter2 else begin let inter1 = eval env e1 in if estexcept inter1 then inter1 else Int ((arithop2fun op) (recupint (inter1)) (recupint inter2)) end  
  | Variable "_" -> failwith "Ordure cosmopolite"
  | Variable s -> recup env s
  | Ifte(e1, e2, e3) -> let inter = eval env e1 in if estexcept inter then inter else begin if (recupbool (eval env e1)) then eval env e2 else eval env e3 end
  | Boolop1 (op, e1, e2) -> let inter2 = eval env e2 in if estexcept inter2 then inter2 else begin let inter1 = eval env e1 in if estexcept inter1 then inter1 else Bool ((boolop12fun op) (recupint inter1) (recupint inter2)) end
  | Boolop2 (op, e1, e2) -> let inter2 = eval env e2 in if estexcept inter2 then inter2 else begin let inter1 = eval env e1 in if estexcept inter1 then inter1 else Bool ((boolop22fun op) (recupbool inter1) (recupbool inter2)) end
  | Non(e) -> let inter = eval env e in if estexcept inter then inter else Bool (not (recupbool inter))
  | Fonction(m,e) ->Fun (env,m,e,None)
  | Letin(s, b, c) -> let inter = eval env b in if estexcept inter then inter else eval ( eval_affectation env s inter)  c
  | Letrec(s, b, c) -> let (x,f) = recupfonc b in
                                eval ( (s,(Fun (env,x,f, Some s))):: env ) c
(* là pour traiter prInt et ref comme des fonctions, on leur associe une valeur de fonctions, où en realité seule importe le dernier argument, qui est Some "1" ou Some "2", car Some contient d'habitude un nom de variables, qui ne peut pas commencer par un chifre donc il n'y a pas d'ambigüité, et donc quand on tombe sur une fonction avec Some "1" ou Some "2" on sait que on a affaire à ce type de fonction.*)
  | Print -> Fun([], "x", Unite, Some "1")
  | Ref -> Fun ([], "x", Unite, Some "2")
  | Appli (e1, e2) -> let v2 = eval env e2 in if estexcept v2 then v2 else begin
                      let a = eval env e1 in if estexcept a then a else begin
                      let (envi,m,f,r)= recupfun a in
                      begin
                        match r with
                          | None -> eval (eval_matching envi m v2) f
                          | Some "1" -> begin print_int (recupint v2); print_newline (); v2 end
                          | Some "2" -> begin incr index; reference.(!index) <- v2; Refv(!index) end
                          | Some "3" -> begin match v2 with
                                          | Tuplev([t1; t2]) -> t1
                                          | _ -> failwith "Mauvaise utilisation de fst."
                                        end
                          | Some "4" -> begin match v2 with
                                          | Tuplev([t1; t2]) -> t2
                                          | _ -> failwith "Mauvaise utilisation de snd."
                                        end
                          | Some s -> eval ((s,a)::(eval_matching envi m v2)) f
                      end end end
  | Valeurref(e1) -> let s = eval env e1 in if estexcept s then s else reference.(recupref s)
  | Changeref(e1, e2) -> let a = eval env e2 in if estexcept a then a else begin let s = eval env e1 in if estexcept s then s else (reference.(recupref s) <- a; Unitv) end
  | Unite -> Unitv
  | Tuple(l) -> let rec aux l = match l with (* Aux sert à parcourir la liste en arrêtant dès que l'on rencontre une exception. *)
                                  | [] -> [] 
                                  | t::q -> let inter = eval env t in if estexcept inter then [inter] else let inter2 = aux q in begin match inter2 with [Except(k)] -> [Except(k)] | _ -> inter::inter2 end
                in let inter3 = aux l in begin match inter3 with | [Except(k)] -> Except(k) | li -> Tuplev(li) end

  | Cons(a, b) -> let inter2 = eval env b in if estexcept inter2 then inter2 else begin let inter1 = eval env a in if estexcept inter1 then inter1 else begin match b with 
    | Listvide -> Consv( inter1, inter2) 
    | Cons(_,_) -> Consv( inter1, inter2) 
    | _ -> failwith "Une liste doit finir par la liste vide" end end
(* L'intérêt de la ligne d'au dessus et de ne regarder que des listes qui ont des têtes de listes, à savoir qu'on fait cons des trucs jusqu'à arriver à cons la liste vide.*)
  | Listvide -> Vide
  | Match(x, l) -> let inter = eval env x in if estexcept inter then inter else let mat = eval_matching(inter,l) in eval ((fst mat) @ env) (snd mat)
  | Fst -> Fun([], "x", Unite, Some "3")
  | Snd -> Fun ([], "x", Unite, Some "4")
  | Raise(e) -> Except(recupint (eval env e))
  | Try(a,b) -> let inter = eval env a in begin
    match inter with
      | Except(k) -> let mat = eval_matching(Int k,b) in eval ((fst mat) @ env) (snd mat)
      | _ -> inter
    end

and

(* Dans eval_affectation, on cherche à faire l'équivalent d'un matching. Pour cela, on va parcourir récurisvement ce qu'on a à gauche et affecter au fur et à mesure des valeurs aux variables en enrichissant petit à petit l'envrionnement. Pour cela, affecte la valeur avec l'environnement modifié du reste. *)
  eval_affectation env s b = match s with
    | Tuplem([]) -> env
    | Tuplem(t::q) -> let l = recuptuple b in if List.length l = List.length (t::q) then eval_affectation (eval_affectation env (Tuplem(q)) (Tuplev(List.tl l))) t (List.hd l) else failwith "On ne peut faire correspondre que des tuples même taille."
    | Videm -> if b = Vide then env else failwith "Si on veut matcher une liste de taille fixer, il faut que en face ca corresponde." (* ce if est là pour empecher let a::[] = [1;2] de marcher. *)
    | Consm(a, q) -> let (c, d) = recupcons b in eval_affectation (eval_affectation env q d) a c
    | Varm ("_") -> env
    | Varm(s1) -> (s1, b) :: env
    | Constm(_) -> failwith "On ne peut pas affecter une valeur à une constante entière."


and eval_matching (x,l) = match l with | [] -> failwith "Le matching n'était pas assez complet." | t :: q -> begin
  let (m, e) = t in

  let rec aux env s b = match s with
    | Consm(a, z) -> begin match b with 
      | Consv(c, d) -> aux (aux env z d ) a c
      | _ -> raise NOMATCH end
    | Videm -> if b = Vide then env else raise NOMATCH
    | Tuplem([]) -> if b = Tuplev([]) then env else raise NOMATCH
    | Tuplem(y::z) -> begin match b with 
      | Tuplev(l) -> if List.length (y::z) = List.length l then aux (aux env (Tuplem(z)) (Tuplev(List.tl l))) y (List.hd l) else raise NOMATCH
      | _ -> raise NOMATCH end
    | Varm ("_") -> env
    | Varm(s1) -> (s1, b) :: env
    | Constm(c) -> if b = Int(c) then env else raise NOMATCH

  in
  try let st = aux [] m x in if doublon (List.map fst st) then failwith "Voyou !" else (st, e)
  with | NOMATCH -> eval_matching (x, q)

end



