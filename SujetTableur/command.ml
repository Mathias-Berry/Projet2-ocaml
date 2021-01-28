open Debug
open Cell
open Sheet

(* commandes: ce que l'utilisateur peut saisir dans un fichier.
 - La modification d'une cellule avec une nouvelle formule,
 - l'affichage d'une cellule, 
 - l'affichage de toute la feuille *)
type comm = Upd of cellname * form | Show of cellname | ShowAll


(************ affichage **************)
let show_comm c =
  match c with
  | Upd (c,f) ->
     begin
       ps (cell_name2string c);
       ps"=";
       show_form f
     end
  | Show c ->
     begin
       ps "Show(";
       ps (cell_name2string c);
       ps ")"
     end
  | ShowAll -> ps "ShowAll"

(************ faire tourner les commandes **************)

(* exécuter une commande *)
let run_command c = match c with
  | Show cn ->
     begin
       let co = cellname_to_coord cn in
       eval_p_debug (fun () ->
           "Showing cell "
           ^ cell_name2string cn
         );
       ps (cell_val2string (read_cell co)); (* <- ici ps, et pas p_debug, car on veut afficher au moins cela *)
       print_newline()
     end
  | ShowAll ->
     begin
       eval_p_debug (fun () -> "Show All\n");
       show_sheet ()
     end
  | Upd(cn,f) ->
     let co = cellname_to_coord cn in
     eval_p_debug (fun () -> "Update cell " ^ cell_name2string cn ^ "\n");
     if liste_dep co co (* c'est dommage qu'on l'ait pas appelé ca ça aurait fait ca ca *)
     then begin
     update_cell_formula co f;
     suppr_dep co;
     rajoute_dep co f
     end;
     recompute_sheet()

(* exécuter une liste de commandes *)
let run_script cs = List.iter run_command cs
