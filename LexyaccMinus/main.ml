open Expr
open Eval

let compile e =
  begin
    affiche_expr e;
    print_newline();
    print_value (eval [] e);
    print_newline()
  end

(* stdin désigne l'entrée standard (le clavier) *)
(* lexbuf est un canal ouvert sur stdin *)

let nom_fichier = ref ""

let lexbuf = Lexing.from_channel stdin

(* on enchaîne les tuyaux: lexbuf est passé à Lexer.token,
   et le résultat est donné à Parser.main *)

let parse () = Parser.main Lexer.token lexbuf

(* la fonction que l'on lance ci-dessous *)
let calc () =
  let usage  ="" in
  let optlist = [("-showsrc", Arg.Set source, "Affiche le programme et enlève les sorties");
                 ("-debug", Arg.Set debug, "Affiche le programme et affiche les sorties")] in
   
  Arg.parse optlist (fun s -> nom_fichier := s) usage;
  
  try
      let code = open_in !nom_fichier
      let result = parse () in
      (* Expr.affiche_expr result; print_newline (); flush stdout *)
	compile result; flush stdout
  with _ -> (print_string "erreur de saisie\n")
;;

let _ = calc()
