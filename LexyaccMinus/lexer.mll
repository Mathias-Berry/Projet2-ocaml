{
  open Parser;;        (* le type "token" est défini dans parser.mli *)
(* ce n'est pas à vous d'écrire ce fichier, il est engendré automatiquement *)
exception Eof;;
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t']     { token lexbuf }    (* on saute les blancs et les tabulations *)
 	     	   	           (* token: appel récursif *)
                                   (* lexbuf: argument implicite
                                      associé au tampon où sont
                                      lus les caractères *)
  | '\n'            { EOL }
  | '+'             { PLUS }
  | '*'             { TIMES }
  | '-'             { MINUS }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | "let"			{ LET }
  | "in"			{ IN }
  | "="				{ EGAL }
  | ['0'-'9']+ as s { INT (int_of_string s) }
  | ['a'-'z']+ as s { STR s }
  | eof             { raise Eof } 
