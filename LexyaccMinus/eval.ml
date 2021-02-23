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
  | Ifte(e1, e2, e3) -> if eval_bool env e1 then eval env e2 else eval env e3
  | Print (a) -> (print_int a;print_newline (); a)
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
