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
  | PVDOUBLE
  | PTV
  | VIRGULE
  | IF
  | THEN
  | ELSE
  | CONS
  | LT
  | LE
  | GT
  | GE
  | AND
  | OR
  | NOT
  | NE
  | EVALREF
  | REF
  | ASS
  | LPAREN
  | RPAREN
  | BEGIN
  | END
  | RCROCH
  | LCROCH
  | FUN
  | TO
  | REC
  | EXCEPTION
  | TRY
  | RAISE
  | MATCH
  | ORMATCH
  | WITH
  | FST
  | SND
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Expr.expr
