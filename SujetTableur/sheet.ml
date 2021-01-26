(* tableau de cellules *)
open Cell

let size = (20,10) (* lignes, colonnes *)

(* le tableau que l'on manipule dans le programme ; *)
(* si nécessaire, tapez "fst" et "snd" dans un interprete Caml pour connaître leur type *) 
(* default_cell est défini dans cell.ml (module Cell) *)
let thesheet = Array.make_matrix (fst size) (snd size) default_cell

let read_cell co = thesheet.(fst co).(snd co)

let update_cell_formula co f = thesheet.(fst co).(snd co).formula <- f
let update_cell_value co v = thesheet.(fst co).(snd co).value <- v


(* exécuter une fonction, f, sur tout le tableau *)
let sheet_iter f =
  for i = 0 to (fst size -1) do
    for j = 0 to (snd size -1) do
      f i j
    done;
  done



(* initialisation du tableau : questions un peu subtiles de partage,
 * demandez autour de vous si vous ne comprenez pas pourquoi cela est
 * nécessaire.  
 * Vous pouvez ne pas appeler la fonction ci-dessous,
 * modifier une case du tableau à l'aide de update_cell_formula, et
 * regarder ce que ça donne sur le tableau : cela devrait vous donner
 * une piste *)
let init_sheet () =
  let init_cell i j =
    let c = { value = None; formula = Cst 0. } in
    thesheet.(i).(j) <- c
  in
  sheet_iter init_cell

(* on y va, on initialise *)
let _ = init_sheet ()


(* affichage rudimentaire du tableau *)

let show_sheet () =
  let g i j =
    begin
       (* aller à la ligne en fin de ligne *)
      if j = 0 then print_newline() else ();
      let c = read_cell (i,j) in
      print_string (cell_val2string c);
      print_string " "
    end
  in
  sheet_iter g;
  print_newline()




(********** calculer les valeurs à partir des formules *************)

(* on marque qu'on doit tout recalculer en remplissant le tableau de "None" *)
(*    à faire : mettre tout le monde à None *)
let invalidate_sheet () = 
  let g i j =
    begin
    thesheet.(i).(j).value <- None
    end
  in
  sheet_iter g


(*    à faire : le cœur du programme *)    
let rec sum_list l =
  match l with
    | []-> 0.
    | t::q-> t +. (sum_list q)

let rec mul_list l =
  match l with
    | []-> 1.
    | t::q-> t *. (sum_list q)

let rec eval_form fo = match fo with
  | Cst n -> n
  | Cell (p,q) -> eval_cell p q
  | Op(o,fs) -> match o with
  		| S -> sum_list (List.map eval_form fs)
  		| M -> mul_list (List.map eval_form fs)
  		| A -> sum_list (List.map eval_form fs)/. (float_of_int(List.length fs))
  		       

(* ici un "and", car eval_formula et eval_cell sont a priori 
   deux fonctions mutuellement récursives *)
and eval_cell i j =
  match thesheet.(i).(j).value with
    | None -> let f = eval_form (thesheet.(i).(j).formula) in
              thesheet.(i).(j).value <- Some f;
              f
    | Some f -> f
  

(* on recalcule le tableau, en deux étapes *)
let recompute_sheet () =
  invalidate_sheet ();
  sheet_iter eval_cell
