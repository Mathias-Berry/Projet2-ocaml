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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
(* --- préambule: ici du code Caml --- *)

open Cell
open Command

# 27 "parser.ml"
let yytransl_const = [|
  260 (* LPAREN *);
  261 (* RPAREN *);
  262 (* EQUAL *);
  263 (* SEMICOL *);
  264 (* DOT *);
  265 (* SUM *);
  266 (* MULT *);
  267 (* AVERAGE *);
  268 (* SHOW *);
  269 (* SHOWALL *);
  270 (* MAX *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* NBR *);
  259 (* CELLROW *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\004\000\006\000\
\006\000\006\000\006\000\005\000\005\000\005\000\005\000\007\000\
\007\000\000\000"

let yylen = "\002\000\
\002\000\002\000\001\000\003\000\002\000\001\000\002\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\004\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\006\000\018\000\000\000\000\000\
\000\000\007\000\005\000\001\000\002\000\000\000\013\000\012\000\
\008\000\009\000\010\000\011\000\014\000\004\000\000\000\000\000\
\000\000\000\000\000\000\015\000\017\000"

let yydgoto = "\002\000\
\006\000\007\000\008\000\021\000\025\000\023\000\026\000"

let yysindex = "\002\000\
\002\255\000\000\005\255\009\255\000\000\000\000\016\000\002\255\
\011\255\000\000\000\000\000\000\000\000\255\254\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\014\255\255\254\
\012\255\015\255\255\254\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\021\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\017\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\015\000\000\000\003\000\010\000\000\000\254\255"

let yytablesize = 25
let yytable = "\015\000\
\016\000\003\000\001\000\009\000\003\000\010\000\011\000\017\000\
\018\000\019\000\009\000\003\000\020\000\004\000\005\000\012\000\
\014\000\024\000\027\000\028\000\003\000\016\000\013\000\022\000\
\029\000"

let yycheck = "\001\001\
\002\001\003\001\001\000\001\000\003\001\001\001\004\000\009\001\
\010\001\011\001\008\000\003\001\014\001\012\001\013\001\000\000\
\006\001\004\001\007\001\005\001\000\000\005\001\008\000\014\000\
\027\000"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  EQUAL\000\
  SEMICOL\000\
  DOT\000\
  SUM\000\
  MULT\000\
  AVERAGE\000\
  SHOW\000\
  SHOWALL\000\
  MAX\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  NBR\000\
  CELLROW\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'clist) in
    Obj.repr(
# 27 "parser.mly"
               ( _1 )
# 124 "parser.ml"
               : Command.comm list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'singlecomm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'clist) in
    Obj.repr(
# 31 "parser.mly"
                      ( _1::_2 )
# 132 "parser.ml"
               : 'clist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'singlecomm) in
    Obj.repr(
# 32 "parser.mly"
                               ( [_1] )
# 139 "parser.ml"
               : 'clist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cell) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 36 "parser.mly"
                        ( Upd(_1,_3) )
# 147 "parser.ml"
               : 'singlecomm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'cell) in
    Obj.repr(
# 37 "parser.mly"
               ( Show(_2) )
# 154 "parser.ml"
               : 'singlecomm))
; (fun __caml_parser_env ->
    Obj.repr(
# 38 "parser.mly"
             ( ShowAll )
# 160 "parser.ml"
               : 'singlecomm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 42 "parser.mly"
                 ( (_1,_2) )
# 168 "parser.ml"
               : 'cell))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
         ( S )
# 174 "parser.ml"
               : 'operand))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
          ( M )
# 180 "parser.ml"
               : 'operand))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
             ( A )
# 186 "parser.ml"
               : 'operand))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
         ( Max )
# 192 "parser.ml"
               : 'operand))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 53 "parser.mly"
         ( Cst _1 )
# 199 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 54 "parser.mly"
         ( Cst (float _1) )
# 206 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cell) in
    Obj.repr(
# 55 "parser.mly"
          ( Cell (Cell.cellname_to_coord _1) )
# 213 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'operand) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'forlist) in
    Obj.repr(
# 56 "parser.mly"
                                   ( Op(_1,_3) )
# 221 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 60 "parser.mly"
             ( [_1] )
# 228 "parser.ml"
               : 'forlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'forlist) in
    Obj.repr(
# 61 "parser.mly"
                             ( _1::_3 )
# 236 "parser.ml"
               : 'forlist))
(* Entry debut *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let debut (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Command.comm list)
