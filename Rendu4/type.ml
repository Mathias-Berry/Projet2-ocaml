open Expr


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

let cota = ref 0
let contraintes = ref []

let append x = contraintes:= x::(!contraintes)


let rec recup e s = match e with
  | [] -> failwith "La variable n'est pas définie"
  | (a, b)::q when a <> s -> recup q s
  | (a, b)::q -> b

let recuptuple = function
  |Tuples l -> l
  |_ -> failwith"pas possible"

let rec addmotenv = function 
    |Varm s-> incr cota; append (!cota,Tout);[(s,Pasdef(!cota))]
    |Consm(m1,m2)-> (addmotenv m1)@(addmotenv m2)                         
    |Videm-> []
    |Tuplem (m1::q1)-> (addmotenv m1)@(addmotenv (Tuplem q1))
    |Tuplem []->[]
    |_ -> failwith"pas des indices de fonctions"

let rec recomposemot env = function
    |Varm s-> recup env s 
    |Consm(m1,m2)-> Liste (recomposemot env m1)
    |Tuplem (m1::q1)->  Tuples ((recomposemot env m1)::(recuptuple (recomposemot env (Tuplem q1))))
    |Tuplem [] -> Tuples []
    |_ -> failwith"pas possible"


let rec typagemot m t =
  match m,t with
    |Varm s,_-> [(s,t)]
    |Consm(m1,m2),Liste t1-> (typagemot m1 t1)@(typagemot m2 t)                         
    |Videm,_-> []
    |Tuplem (m1::q1),Tuples (t1::q2) -> (typagemot m1 t1)@(typagemot (Tuplem q1) (Tuples q2))
    |Tuplem [],Tuples [] ->[]
    |_,_ -> failwith"pas typable"


let rec typage env = function
  | Const k -> Inte
  | Unite -> Unit
  | Variable s -> recup env s
  | Tuple(l) -> Tuples (List.map (typage env) l)
  | Listvide -> incr cota; append (!cota,Tout); Liste(Pasdef (!cota))
  | Cons(e1, e2) -> let temp1 = typage env e1 in 
            let temp2 = typage env e2 in
            incr cota; append (!cota,temp1);
            incr cota; append (!cota,temp2);append (!cota, Liste (Pasdef ((!cota)-1))); temp2
  | Arithop(op,e1,e2) -> incr cota; append (!cota,typage env e1); append(!cota,Inte); incr cota; append (!cota,typage env e2); append(!cota,Inte); Inte 
  | Ifte(e1, e2, e3) -> let temp1 = typage env e1 in 
              let temp2 = typage env e2 in
              let temp3 = typage env e3 in 
              incr cota; append (!cota,temp1); append (!cota, Boole);incr cota; append (!cota,temp2); incr cota; append (!cota,temp3); append (!cota,Pasdef ((!cota)-1)); temp2
  | Boolop1 (op, e1, e2) -> incr cota; append (!cota,typage env e1); append(!cota,Inte); incr cota; append (!cota,typage env e2); append(!cota,Inte); Boole
  | Boolop2 (op, e1, e2) -> incr cota; append (!cota,typage env e1); append(!cota,Boole); incr cota; append (!cota,typage env e2); append(!cota,Boole); Boole
  | Non(e) -> incr cota; append (!cota,typage env e); append(!cota,Boole); Boole 
  | Valeurref(e) -> incr cota; append (!cota, Tout);incr cota; append (!cota,typage env e); append (!cota, Reff (Pasdef((!cota) -1)));Pasdef((!cota) -1) 
  | Changeref(e1, e2) ->let temp2 = typage env e2 in
              incr cota; append (!cota,typage env e2); incr cota; append (!cota,temp2); append (!cota, Reff (Pasdef ((!cota)-1))); Unit
  | Raise(e) -> let temp = typage env e in
          incr cota; append (!cota, temp); append (!cota, Inte);Tout
  | Letin(s, b, c) -> let temp1 = typage env b in
                      typage ((typagemot s temp1)@env) c
  | Letrec(s, b, c) -> let temp1 = typage env b in
                       typage ((typagemot (Varm s) temp1)@env) c
  | Print -> Fonc (Inte,Inte)
  | Ref -> incr cota; Fonc (Pasdef (!cota),Pasdef (!cota))
  | Fst -> incr cota; Fonc (Pasdef (!cota),Pasdef (!cota))
  | Snd -> incr cota; Fonc (Pasdef (!cota),Pasdef (!cota))
  | Appli (e1, e2) ->let temp1 = typage env e1 in 
                     let temp2 = typage env e2 in
                     incr cota; append (!cota,temp1); incr cota; append (!cota,temp2); incr cota;append ((!cota)-1,Fonc (Pasdef(!cota),Pasdef(!cota+1))); incr cota;Pasdef(!cota)
  | Match(x, (m,e)::q) -> let temp1 = typage ((addmotenv m)@env) e in
                          let temp2 = typage env (Match (x,q)) in
                          incr cota; append (!cota,temp1); incr cota; append (!cota,temp2); append (!cota,Pasdef ((!cota)-1)); temp2
  | Fonction(m,e) -> let temp1 = typage ((addmotenv m)@env) e in
                     let temp2 = recomposemot env m in
                     Fonc (temp2,temp1)



let chybraltar e =
  let t = typage [] e in
  !contraintes 