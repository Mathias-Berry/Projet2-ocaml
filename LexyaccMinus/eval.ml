open Expr

type envi = (string*int) list

let rec recup e s = match e with
	| [] -> failwith "La variable n'est pas définie"
	| (a, b)::q when a <> s -> recup q s
	| (a, b)::q -> b


let rec eval env = function
  | Const k -> k
  | Add(e1,e2) -> (eval env e1) + (eval env e2)
  | Mul(e1,e2) -> (eval env e1) * (eval env e2)
  | Min(e1,e2) -> (eval env e1) - (eval env e2)
  | Letin(Variable s, b, c) -> eval ( (s, eval env b):: env ) c
  | Variable s -> recup env s
  | _ -> failwith "bite"

