type token =
  | INT of (int)
  | NBR of (float)
  | CELLROW of (string)
  | LPAREN
  | RPAREN
  | EQUAL
  | SEMICOL
  | DOT
  | SUM
  | MULT
  | AVERAGE
  | SHOW
  | SHOWALL
  | MAX
  | EOF

val debut :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Command.comm list
