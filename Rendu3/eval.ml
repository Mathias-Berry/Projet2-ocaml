open Expr


type envi = (string*value) list
and value = Int of int | Fun of envi*string*expr*(string option) | Bool of bool | Ref of int | Unitv

let reference = Array.make 1000 (Int(0))

let index = ref 0

let print_value x =
  match x with
    |Int k -> print_int k
    |Bool b -> if b then print_string "true" else print_string "false"
    |Fun (e,x,f,r)-> print_string ("fun "^x^" -> "); affiche_expr f
    |Unitv -> ()
    |Ref(_) -> failwith "Pas affichable"



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
    | Ref b -> b
    | _ -> failwith "l'argument n'est pas une référence"

let recuprunit v =
  match v with
    | Unite -> Unitv
    | _ -> failwith "L'argument n'est pas de type unit"

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



let rec eval env = function
  | Const k -> Int k
  | Arithop(op,e1,e2) -> Int ((arithop2fun op) (recupint (eval env e2)) (recupint (eval env e1)))
  | Variable s -> recup env s
  | Ifte(e1, e2, e3) -> if (recupbool (eval env e1)) then eval env e2 else eval env e3
  | Boolop1 (op, e1, e2) -> Bool ((boolop12fun op) (recupint (eval env e1)) (recupint (eval env e2)))
  | Boolop2 (op, e1, e2) -> Bool ((boolop22fun op) (recupbool (eval env e1)) (recupbool (eval env e2)))
  | Non(e) ->Bool (not (recupbool (eval env e)))
  | Fonction(x,e) ->Fun (env,x,e,None)
  | Letin(s, b, c) -> eval ( (s,(eval env b)):: env ) c
  | Letrec(s, b, c) -> let (x,f) = recupfonc b in
                                eval ( (s,(Fun (env,x,f, Some s))):: env ) c
  | Print (a) -> (let b = eval env a in
                  if (not (!source)) then 
                    begin
                      print_int (recupint b);print_newline ()
                    end;
                  b)
  | Appli (e1, e2) -> let v2 = eval env e2 in
                      let a = eval env e1 in
                      let (envi,x,f,r)= recupfun a in
                      if r = None then
                        begin
                          eval ((x,v2)::envi) f
                        end
                      else
                        begin
                          let s = recupsome r in
                          eval ((s,a)::(x,v2)::envi) f
                        end
  | Pv(e1, e2) -> begin let _ = eval env e1 in eval env e2 end
  | Ref(e1) -> begin incr index; reference.(!index) <- (eval env e1); Ref(!index) end
  | Valeurref(e1) -> let s = eval env e1 in reference.(recupref s)
  | Changeref(e1, e2) -> let s = eval env e1 in let a = eval env e2 in (reference.(recupref s) <- a; Unitv)
  | Unite -> Unitv



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
