open Cell
open Sheet
open Command

(*** d�but de la partie "incantatoire" ***)
(* stdin d�signe l'entr�e standard (le clavier) *)
(* lexbuf est un canal ouvert sur stdin *)
let lexbuf = Lexing.from_channel stdin

(* on encha�ne les tuyaux: lexbuf est pass� � Lexer.token,
   et le r�sultat est donn� � Parser.main *)

let parse () = Parser.debut Lexer.token lexbuf
(*** fin de la partie "incantatoire" ***)

let spreadsheet () =
		  try
			let usage = "Test" in
			let optlist = [("-paf", Arg.Set paf, "Et paf ! Pr�sent ! Non on ne t'appelle pas Epaf.")] in
			Arg.parse optlist (fun s -> ()) usage;
			
      let result = parse () in
      begin
        (* le seul endroit a comprendre (dans un premier temps) :
           appel a la fonction run_script, qui est definie dans command.ml *)
        run_script result;
        flush stdout;
      end
      with PAF -> begin print_string "PAF"; print_newline() end
;;


let _ = spreadsheet()

(* let _ = let scr = [ Upd (("C",2), Cst 3.7); ShowAll ] in
 *         begin
 *           print_string "HAHAHA!!!\n";
 *           run_script scr
 *         end *)
