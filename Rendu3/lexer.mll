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
  | "E"             { EXCEPTION }
  | "try"           { TRY }
  | "raise"         { RAISE }
  | "match"         { MATCH }
  | "with"          { WITH }
  | "let"           { LET }
  | "in"            { IN }
  | "="             { EGAL }
  | "if"            { IF } 
  | "else"          { ELSE }
  | "then"          { THEN }
  | "<>"            { NE }
  | "<="            { LE }
  | ">="            { GE }
  | "<"             { LT }
  | ">"             { GT }
  | "not"           { NOT }
  | "&&"            { AND }
  | "||"            { OR }
  | "|"             { ORMATCH }
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
  | "["             { LCROCH }
  | "]"             { RCROCH}
  | "::"            { CONS }
  | ['0'-'9']+ as s { INT (int_of_string s) }
  | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9']* | _ as s { STR s }
