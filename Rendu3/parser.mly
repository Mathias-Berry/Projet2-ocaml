%{
(* --- préambule: ici du code Caml --- *)

open Expr  

%}
/* description des lexèmes, ceux-ci sont décrits (par vous) dans lexer.mll */

%token <int> INT       /* le lexème INT a un attribut entier */
%token <string> STR
%token PRINT
%token PLUS TIMES DIV MINUS
%token LET IN EGAL PVDOUBLE PTV UNIT VIRGULE
%token IF THEN ELSE CONS
%token LT LE GT GE AND OR NOT NE
%token EVALREF REF ASS
%token LPAREN RPAREN BEGIN END RCROCH LCROCH
%token FUN TO REC EXCEPTION TRY RAISE
%token MATCH ORMATCH WITH FST SND
%token EOF             /* Fin de fichier */

 
%nonassoc PLUSFAIBLE TUPLES LISTEP
%left ELSE IN TO
%right PTV 
%left VIRGULE
%nonassoc ASS
%left PLUS MINUS  /* associativité gauche: a+b+c, c'est (a+b)+c */
%left TIMES  DIV/* associativité gauche: a*b*c, c'est (a*b)*c */
%right CONS
%left LE GE AND OR EGAL GT NE LT

%nonassoc FUNPRE REF 

%nonassoc UMINUS  /* un "faux token", correspondant au "-" unaire */
                  /* cf. son usage plus bas : il sert à "marquer" une règle pour lui donner la précédence maximale */
%nonassoc EVALREF
%nonassoc UNIT
%left NOT PRINT
%nonassoc ATOME 
%nonassoc ORMATCH
%nonassoc PLUSFORT EXCEPTION
%nonassoc LPAREN RPAREN INT STR BEGIN END RCROCH
%start main             /* "start" signale le point d'entrée: */
                        /* c'est ici main, qui est défini plus bas */
%type <Expr.expr> main     /* on _doit_ donner le type associé au point d'entrée */

%%
    /* --- début des règles de grammaire --- */
                            /* à droite, les valeurs associées */


main:                       /* <- le point d'entrée (cf. + haut, "start") */
expression_init EOF                { $1 }  /* on veut reconnaître une expression */
  ;


expression_init:
  | expression                                            { $1 }
  | LET motif EGAL expression PVDOUBLE expression_init    { Letin($2,$4,$6) }
;

  expression:         /* règles de grammaire pour les expressions */
  | atomique %prec ATOME                           { $1 }
  | IF expression THEN expression ELSE expression  { Ifte($2,$4,$6) }
  | LET strlist2 EGAL expression IN expression     { Letin(Varm (List.hd $2), List.fold_right (fun x expr -> Fonction(x, expr)) (List.tl $2) $4, $6) }
  | LET motif EGAL expression IN expression        { Letin($2,$4,$6) }
  | FUN strlist TO expression                      { List.fold_right (fun x expr -> Fonction(x, expr)) $2 $4 }
  | expression PLUS expression                     { Arithop(Add,$1,$3) }
  | expression TIMES expression                    { Arithop(Mul,$1,$3) }
  | expression DIV expression                      { Arithop(Div,$1, $3) }
  | expression MINUS expression                    { Arithop(Min,$1,$3) }
  | MINUS expression %prec UMINUS                  { Arithop(Min,Const 0, $2) }
  | expression LE expression                       { Boolop1(Le,$1,$3) }
  | expression GE expression                       { Boolop1(Ge,$1,$3) }
  | expression AND expression                      { Boolop2(And,$1,$3) }
  | expression OR expression                       { Boolop2(Or,$1,$3) }
  | expression EGAL expression                     { Boolop1(Eg,$1,$3) }
  | expression NE expression                       { Boolop1(Ne,$1,$3) }
  | expression GT expression                       { Boolop1(Gt,$1,$3) }
  | expression LT expression                       { Boolop1(Lt,$1,$3) }
  | NOT expression                                 { Non($2) }
  | expression atomique %prec FUNPRE               { Appli($1, $2) }
  | LET REC strlist EGAL expression IN expression  { Letrec(List.hd $3, List.fold_right (fun x expr -> Fonction(x, expr)) (List.tl $3) $5, $7) }
  | EVALREF atomique                               { Valeurref($2) }
  | expression ASS expression                      { Changeref($1,$3) }
  | tuples %prec TUPLES                            { Tuple(List.rev($1)) }
  | expression CONS expression                     { Cons($1,$3)}
  | PRINT                                          { Print }
  | REF                                            { Ref }
  | expression PTV expression                             { Letin(Varm("_"),$1,$3) }
  | MATCH expression WITH matching                 { Match($2,$4) }
  | MATCH expression WITH ORMATCH matching         { Match($2,$5) }
  | RAISE EXCEPTION expression                     { Raise($3) }
  | TRY expression WITH matchex                    { Try($2,$4) }
  | TRY expression WITH ORMATCH matchex            { Try($2,$5) }
  | liste                                          { $1 }
  | SND                                            { Snd }
  | FST                                            { Fst }
;

  atomique:
  | BEGIN expression END                           { $2 }
  | LPAREN expression RPAREN                       { $2 }
  | INT                                            { Const $1 }
  | STR                                            { Variable $1 }
  | LPAREN RPAREN                                  { Unite }
 ;

  strlist2:
  | STR STR strlist                                { $1 :: $2 :: $3 }
  | STR STR                                        { [$1; $2] }
;

  strlist:
  | STR                                            { [$1] }
  | STR strlist                                    { $1 :: $2 }
;

 motif:
  | INT                                             { Constm($1) }
  | STR                                             { Varm($1) }
  | tuplem %prec TUPLES                             { Tuplem (List.rev($1))}
  | motif CONS motif                                { Consm($1,$3) }
  | LPAREN motif RPAREN                             { $2 }
  | BEGIN motif END                                 { $2 }
  | listem                                          { $1 }
;

  tuplem:
  | tuplem VIRGULE motif                           { $3::$1 }    
  | motif VIRGULE motif                            { [$3;$1] }
;

  tuples:
  | tuples VIRGULE expression                      { $3::$1 }    
  | expression VIRGULE expression                  { [$3;$1] }
;


  matching:
    |  motif TO expression ORMATCH matching          { ($1,$3)::$5 }
    |  motif TO expression                           { [($1,$3)] }
;


  matchex:
    |  EXCEPTION atomique TO expression ORMATCH matchex            { ($2,$4)::$6 }
    |  EXCEPTION atomique TO expression                            { [($2,$4)] }
;


  liste:
  | LCROCH RCROCH                                                    { Listvide }
  | LCROCH elts                                                      {$2}
;

  elts :
  | expression  RCROCH                                              { Cons($1,Listvide) }
  | expression PTV elts %prec LISTEP                                            { Cons($1,$3) }
;

  listem:
  | LCROCH RCROCH                                                    { Videm }
  | LCROCH eltsm                                                     {$2}
;

  eltsm :
  | motif PTV eltsm %prec LISTEP                                { Consm($1,$3) }
  | motif  RCROCH                                               { Consm($1,Videm) }
;
