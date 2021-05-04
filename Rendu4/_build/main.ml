open Expr
open Eval
open Type
open Resoud

let compile e =
    begin if !source || !debug then (affiche_expr e; print_newline()) end;

let _ = eval [] e in ()


let nom_fichier = ref ""

(* la fonction que l'on lance ci-dessous *)
let calc () =
  let usage  ="" in
  let optlist = [("-showsrc", Arg.Set source, "Affiche le programme et enlève les sorties");
                 ("-debug", Arg.Set debug, "Affiche le programme et affiche les sorties");
                 ("-notypes", Arg.Set notypes, "Desative l'inference de types");
                 ("-showtypes", Arg.Set showtypes, "Affiche les types de toutes les variables qui entre en jeu")] in
   
  Arg.parse optlist (fun s -> nom_fichier := s) usage;
  
      let code = open_in !nom_fichier in
      let lexbuf = Lexing.from_channel code in
      let parse () = Parser.main Lexer.token lexbuf in
      let result = parse () in
      (* Expr.affiche_expr result; print_newline (); flush stdout *)
    (try if not !notypes then begin
    			let a, b = chybraltar result in 
    			let a = resolution a in
    				if !showtypes then
    				let rec parcours l = match l with
    					| [] -> ()
    					| (x,y)::q -> parcours l; print_string x; print_string " : "; affiche_type a y; print_newline ()
    					in parcours b
    				end
    with Erreur -> print_string " Le typage est pas le bon.");
	compile result; flush stdout

;;

let _ = calc()
