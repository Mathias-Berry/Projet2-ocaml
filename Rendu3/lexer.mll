{
  open Parser;;
exception Eof;;
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t' '\n']     { token lexbuf }    (* on saute les blancs, les tabulations et les sauts de lignes*)


  | eof             { EOF }
  | '+'             { PLUS }
  | '*'             { TIMES }
  | '/'             { DIV }
  | '-'             { MINUS }
  | "()"            { UNIT }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | ','             { VIRGULE }
  | "match"         { MATCH }
  | "|"             { ORMATCH }
  | "with"          { WITH }
  | "let"           { LET }
  | "in"            { IN }
  | "="             { EGAL }
  | "if"            { IF } 
  | "else"          { ELSE }
  | "then"          { THEN }
  | "<="            { LE }
  | ">="            { GE }
  | "<"             { LT }
  | "<>"            { NE }
  | ">"             { GT }
  | "not"           { NOT }
  | "&&"            { AND }
  | "||"            { OR }
  | "prInt"         { PRINT }
  | "fun"			      { FUN }
  | "->"      			{ TO }
  | "rec"           { REC }
  | ";;"            { PVDOUBLE }
  | ";"             { PTV }
  | "begin"         { BEGIN }
  | "end"           { END }
  | "!"             { EVALREF }
  | "ref"           { REF }
  | ":="            { ASS }
  | "[]"            { LISTVIDE }
  | "::"            { CONS }
  | ['0'-'9']+ as s { INT (int_of_string s) }
  | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9']* | _ as s { STR s }
