open Expr
open Eval

let compile e =
  begin
    begin if !source || !debug then (affiche_expr e; print_newline()) end;
    print_value (eval [] e);
    print_newline()
  end


let nom_fichier = ref ""

(* la fonction que l'on lance ci-dessous *)
let calc () =
  let usage  ="" in
  let optlist = [("-showsrc", Arg.Set source, "Affiche le programme et enlève les sorties");
                 ("-debug", Arg.Set debug, "Affiche le programme et affiche les sorties")] in
   
  Arg.parse optlist (fun s -> nom_fichier := s) usage;
  
  try
      let code = open_in !nom_fichier in
      let lexbuf = Lexing.from_channel code in
      let parse () = Parser.main Lexer.token lexbuf in
      let result = parse () in
      (* Expr.affiche_expr result; print_newline (); flush stdout *)
	compile result; flush stdout
  with _ -> (print_string "erreur de saisie\n")
;;

let _ = calc()
