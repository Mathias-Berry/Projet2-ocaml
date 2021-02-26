open Expr

type envi = (string*int) list

let rec recup e s = match e with
	| [] -> failwith "La variable n'est pas définie"
	| (a, b)::q when a <> s -> recup q s
	| (a, b)::q -> b

let recupfonc  e =
  match e with
    | Fonction (x,f)-> (x,f)
    | _ -> failwith"e n'est pas une fonction"

let rec remplce x e f =
  match f with
  | Const k -> Const k
  | Add(e1,e2) -> Add(remplce x e e1,remplce x e e2)
  | Mul(e1,e2) -> Mul(remplce x e e1,remplce x e e2)
  | Div(e1,e2) -> Div(remplce x e e1,remplce x e e2)
  | Min(e1,e2) -> Min(remplce x e e1,remplce x e e2)
  | Variable s -> if Variable s = x then e else Variable s
  | Letin (a, b, c) -> Letin(a, remplce x e b, remplce x e c)
  | Ifte (a,b,c) -> Ifte(remplce x e a,remplce x e b, remplce x e c)
  | Lt (a,b) -> Lt(remplce x e a,remplce x e b)
  | Le (a,b) -> Le(remplce x e a,remplce x e b)
  | Gt (a,b) -> Gt(remplce x e a,remplce x e b)
  | Ge (a,b) -> Ge(remplce x e a,remplce x e b)
  | Eg (a,b) -> Eg(remplce x e a,remplce x e b)
  | And (a,b) -> And(remplce x e a,remplce x e b)
  | Or (a,b) -> Or(remplce x e a,remplce x e b)
  | Non (a) -> Non(remplce x e a)
  | Print (a) -> Print(remplce x e a)
  | Fonction (a,b) -> Fonction(remplce x e a,remplce x e b)
  | Appli (a,b) -> Appli(remplce x e a,remplce x e b)


let rec eval env = function
  | Const k -> k
  | Add(e1,e2) -> (eval env e1) + (eval env e2)
  | Mul(e1,e2) -> (eval env e1) * (eval env e2)
  | Min(e1,e2) -> (eval env e1) - (eval env e2)
  | Div(e1,e2) -> (eval env e1) / (eval env e2)
  | Letin(Variable s, b, c) -> eval ( (s, eval env b):: env ) c
  | Variable s -> recup env s
  | Ifte(e1, e2, e3) -> if eval_bool env e1 then eval env e2 else eval env e3
  | Print (a) -> (let b = eval env a in
                  print_int b;print_newline (); b)
  | Appli (e1, e2) -> let (x,f) = recupfonc e1 in
                      eval env (remplce x e2 f)

  | _ -> failwith "bite"

and eval_bool env = function
  | Lt(e1, e2) -> (eval env e1) < (eval env e2)
  | Le(e1, e2) -> (eval env e1) <= (eval env e2)
  | Eg(e1, e2) -> (eval env e1) = (eval env e2)
  | Ge(e1, e2) -> (eval env e1) >= (eval env e2)
  | Gt(e1, e2) -> (eval env e1) > (eval env e2)
  | Or(e1, e2) -> (eval_bool env e1) || (eval_bool env e2)
  | And(e1, e2) -> (eval_bool env e1) && (eval_bool env e2)
  | Non(e) -> not (eval_bool env e)
  | _ -> failwith "Stade toulousain champion de france"
