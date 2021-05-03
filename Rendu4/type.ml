open Expr

exception ErreurdeTypage of types*types


type types =
  |Inte
  |Boole
  |Unit
  |Tout
  |Fonc of types*types
  |Liste of types
  |Tuples of types list
  |Reff of types
  |Pasdef of int



type cot = I of int | T of types

let cota = ref 0 in
let tab = Array.make 1000 (T Tout)


let rec find x = match tab.(x) with
	|T k -> k,x
	|I k -> find k

let rec addmotif2env m env =
	match m with
		|Varm m -> incr cota; (m,Pasdef(!cota))::env
		|Tuplem (t::q)-> incr cota; (t,Pasdef(!cota))::(addmotif2env (Tuplem(q)) env)
 		|Tuplem [] -> env
 		|Consm (m1,m2) -> ??????? 
 		| _ -> failwith "ce n'est pas une variable" 



let rec egal t1 t2 =
	match t1,t2 with
		|(t1,t2) when t1=t2 -> true
		|(Pasdef t11, Pasdef t22)-> begin
									let (temp1,t1f) = find t11 in 
									let (temp2,t2f) = find t22 in
									if temp1 = Tout then (tab.(t1f)<- I t2f; true)
									else if temp2 = Tout then (tab.(t2f)<- I t1f; true)
									else (egal temp1 temp2)
									end
		|(pasdef t11,t2)-> let (temp1,t1f) = find t11 in 
						   if temp1 = Tout then (tab.(t1f)<- t2; true)
						   else (egal temp1 t2)
		|(t1,pasdef t22)-> let (temp2,t2f) = find t22 in 
						   if temp2 = Tout then (tab.(t2f)<- t1; true)
						   else (egal temp2 t1)
		|(Fonc t11, Fonc t22) -> egal t11 t22
		|(Liste t11, Liste t22) -> egal t11 t22
		|(Tuples l1, Tuples l2) -> egallist l1 l2
		|_->false

and
let egallist l1 l2 =
	match l1,l2 with
		|[],[] -> true
		|t1::q1,t2::q2-> (egal t1 t2) && (egallist q1 q2)
		|_ -> false 



let rec tousidentique l =
	match l with
		|[]-> incr cota; Liste (Pasdef (!cota))
		|t::[] -> t
		|t1::t2::q -> if not (egal t1 t2) then raise ErreurdeTypage(t2,t1) else tousidentique t2::q


let rec recup e s = match e with
  | [] -> failwith "La variable n'est pas définie"
  | (a, b)::q when a <> s -> recup q s
  | (a, b)::q -> b

let recupref t =
  match t with 
   | Reff a -> a
   | _ -> incr cota; raise ErreurdeTypage (t,Reff (Pasdef(!cota)))

let recuplist t =
  match t with 
   | List l -> l
   | _ ->incr cota; raise ErreurdeTypage (t,Liste (Pasdef(!cota)))

let recupfonc t =
  match t with 
   | Fonc t1,t2 -> t1,t2
   | _ -> incr cota; incr cota; raise ErreurdeTypage (t,Fonc(Pasdef((!cota)-1),Pasdef(!cota)))


let rec typage env = function
  | Const k -> Inte
  | Unite -> Unit
  | Variable s -> recup env s
  | Tuple(l) -> Tuples (List.map (typage env) l)
  | Listvide -> incr cota; Liste(Pasdef (!cota))
  | Cons(e1, e2) -> let temp1 = typage env e1 in 
  					let temp2 = recuplist (typage env e2) in
  					if not(egal temp1 temp2) then raise ErreurdeTypage (temp1,temp2)
  						 else Liste temp1
  | Arithop(op,e1,e2) -> let temp1 = typage env e1 in 
						 let temp2 = typage env e2 in
  						 if not (egal temp1 Inte) then raise ErreurdeTypage (temp1,Inte)
  						 else if not (egal temp2 Inte) then raise ErreurdeTypage (temp2,Inte)
  						 else Inte 
  | Ifte(e1, e2, e3) -> let temp1 = typage env e1 in 
  						let temp2 = typage env e2 in 
  						let temp3 = typage env e3 in 
  						if not (egal temp1 Boole) then raise ErreurdeTypage (temp1,Boole)
  						else if not (egal temp2 temp3) then raise ErreurdeTypage (temp2,temp3)
  						else temp2 
  | Boolop1 (op, e1, e2) -> let temp1 = typage env e1 in 
						 	let temp2 = typage env e2 in
  							if not (egal temp1 Inte) then raise ErreurdeTypage (temp1,Inte)
  							else if not (egal temp2 Inte) then raise ErreurdeTypage (temp2,Inte)
  							else Boole 
  | Boolop2 (op, e1, e2) -> let temp1 = typage env e1 in 
						 	let temp2 = typage env e2 in
  						 	if not (egal temp1 Boole) then raise ErreurdeTypage (temp1,Boole)
  							else if not (egal temp2 Boole) then raise ErreurdeTypage (temp2,Boole)
  							else Boole 
  | Non(e) -> let temp = typage env e in 
  			  if not (egal temp Boole) then raise ErreurdeTypage (temp,Boole)
  			  else Boole 
  | Valeurref(e) -> recupref (typage env e)
  | Changeref(e1, e2) -> let temp1 = recupref (typage env e1) in 
  						 let temp2 = typage env e2 in 
  						 if not (egal temp1 temp2) then raise ErreurdeTypage (temp1,temp2)
  						 else Unit 
  | Raise(e) -> let temp = typage env e in
  				if not (egal temp Inte) then raise ErreurdeTypage (temp,Inte) else ???????
  | Match(x, l) -> tousidentique (List.map (typage env) l)
  | Letin(s, b, c) -> let temp1 = typage env b in 
					  typage ((s,temp)::env) c
  | Letrec(s, b, c) -> let temp1 = typage env b in 
					   typage ((s,temp)::env) c
  | Fonction(m,e) -> incr cota;
  | Print -> Fonc (Inte,Inte)
  | Ref -> incr cota; Fonc (Pasdef (!cota),Pasdef (!cota))
  | Fst -> incr cota; Fonc (Pasdef (!cota),Pasdef (!cota))
  | Snd -> incr cota; Fonc (Pasdef (!cota),Pasdef (!cota))
  | Appli (e1, e2) -> let temp11,temp12 = recupfonc (typage env e1) in 
  					  let temp2 = typage env e2 in
  					  if not (egal temp11 temp2) then raise ErreurdeTypage(temp2,temp11)
  					  else temp12 

