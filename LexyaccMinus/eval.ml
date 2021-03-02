open Expr

type envi = (string*value) list
and value = Int of int | Fun of envi*string*expr*(string option)

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

let boolop22fun = function
  | Or -> fun x y -> x || y
  | And -> fun x y -> x && y

let rec recup e s = match e with
	| [] -> failwith "La variable n'est pas définie"
	| (a, b)::q when a <> s -> recup q s
	| (a, b)::q -> b

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
    | Fun (env,x,f)-> (env,x,f)
    | _ -> failwith"v n'est pas une fonction"

let recupint v =
  match v with
    | Int k-> k
    | _ -> failwith"v n'est pas un entier"

let estfun e = 
  match e with
    |Fonction(_,_) -> true
    |_ -> false


let rec eval env = function
  | Const k -> k
  | Arithop(op,e1,e2) -> (arithop2fun op) (eval env e1) (eval env e2)
  | Letin(s, b, c) -> if estfun b then 
                                begin
                                  let (x,f)= recupfonc b in
                                  eval ( (s,Fun (env,x,f)):: env ) c
                                end
                               else
                               begin
                                eval ( (s,Int  (eval env b)):: env ) c
                               end
  | Letrec(s, b, c) -> let (x,f) = recupfonc b in
                                eval ( (s,Fun (env,x,f)):: env ) c
  | Variable s -> recupint (recup env s)
  | Ifte(e1, e2, e3) -> if eval_bool env e1 then eval env e2 else eval env e3
  | Print (a) -> (let b = eval env a in
                  print_int b;print_newline (); b)
  | Appli (e1, e2) -> if estfun e1 then
                        begin
                          let v2 = eval env e2 in
                          let (x,f) = recupfonc e1 in
                          eval ((x,v2)::env) f
                         end
                      else 
                        begin
                          let v = recupvar e1 in
                          let a = recup env v in
                          let (envi,x,f) = recupfun a in
                          let v2 = eval env e2 in
                          eval ((x,v2)::envi) f
                        end
  | _ -> failwith "bite"

and eval_bool env = function
  | Boolop1 (op, e1, e2) -> (boolop12fun op) (eval env e1) (eval env e2)
  | Boolop2 (op, e1, e2) -> (boolop22fun op) (eval_bool env e1) (eval_bool env e2)
  | Non(e) -> not (eval_bool env e)
  | _ -> failwith "Stade toulousain champion de france"
