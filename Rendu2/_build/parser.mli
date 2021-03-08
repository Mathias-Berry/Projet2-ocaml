type token =
  | INT of (int)
  | STR of (string)
  | PRINT
  | PLUS
  | TIMES
  | DIV
  | MINUS
  | LET
  | IN
  | EGAL
  | IF
  | THEN
  | ELSE
  | LT
  | LE
  | GT
  | GE
  | AND
  | OR
  | NOT
  | NE
  | LPAREN
  | RPAREN
  | FUN
  | TO
  | REC
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Expr.expr
