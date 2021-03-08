%{
(* --- préambule: ici du code Caml --- *)

open Expr   (* rappel: dans expr.ml: 
             type expr = Const of int | Add of expr*expr | Mull of expr*expr *)

%}
/* description des lexèmes, ceux-ci sont décrits (par vous) dans lexer.mll */

%token <int> INT       /* le lexème INT a un attribut entier */
%token <string> STR
%token PRINT
%token PLUS TIMES DIV MINUS
%token LET IN EGAL
%token IF THEN ELSE
%token LT LE GT GE AND OR NOT NE
%token LPAREN RPAREN
%token FUN TO REC
%token EOL             /* retour à la ligne */


%left PRINT ELSE IN TO

%left PLUS MINUS  /* associativité gauche: a+b+c, c'est (a+b)+c */
%left TIMES  DIV/* associativité gauche: a*b*c, c'est (a*b)*c */

%left LE GE AND OR EGAL GT NE LT

%nonassoc FUNPRE

%nonassoc UMINUS  /* un "faux token", correspondant au "-" unaire */
                  /* cf. son usage plus bas : il sert à "marquer" une règle pour lui donner la précédence maximale */
%left NOT
%nonassoc ATOME
%nonassoc LPAREN RPAREN INT STR
%start main             /* "start" signale le point d'entrée: */
                        /* c'est ici main, qui est défini plus bas */
%type <Expr.expr> main     /* on _doit_ donner le type associé au point d'entrée */

%%
    /* --- début des règles de grammaire --- */
                            /* à droite, les valeurs associées */


main:                       /* <- le point d'entrée (cf. + haut, "start") */
expression EOL                { $1 }  /* on veut reconnaître une expression */
  ;
  

  expression:			    /* règles de grammaire pour les expressions */

	| atomique %prec ATOME					 								{ $1 }
  | IF expression THEN expression ELSE expression { Ifte($2,$4,$6) }
  | LET strlist EGAL expression IN expression		  { Letin(List.hd $2, List.fold_right (fun x expr -> Fonction(x, expr)) (List.tl $2) $4, $6) }
  | FUN strlist TO expression											{ List.fold_right (fun x expr -> Fonction(x, expr)) $2 $4 }
  | expression PLUS expression                    { Arithop(Add,$1,$3) }
  | expression TIMES expression                   { Arithop(Mul,$1,$3) }
  | expression DIV expression										  { Arithop(Div,$1, $3) }
  | expression MINUS expression                   { Arithop(Min,$1,$3) }
  | MINUS expression %prec UMINUS 								{ Arithop(Min,Const 0, $2) }
  | expression LE expression                      { Boolop1(Le,$1,$3) }
	| expression GE expression                      { Boolop1(Ge,$1,$3) }
  | expression AND expression                     { Boolop2(And,$1,$3) }
  | expression OR expression                      { Boolop2(Or,$1,$3) }
  | expression EGAL expression                    { Boolop1(Eg,$1,$3) }
  | expression NE expression                      { Boolop1(Ne,$1,$3) }
  | expression GT expression                      { Boolop1(Gt,$1,$3) }
  | NOT expression                                { Non($2) }
  | PRINT expression                              { Print($2) }
	| expression atomique %prec FUNPRE							{ Appli($1, $2) }
	| LET REC strlist EGAL expression IN expression { Letrec(List.hd $3, List.fold_right (fun x expr -> Fonction(x, expr)) (List.tl $3) $5, $7) }
;

	atomique:
	| LPAREN expression RPAREN											{ $2 }
	| INT 																					{ Const $1 }
	| STR 																					{ Variable $1 }
;

	strlist:
	| STR																						{ [$1] }
	| STR strlist																		{ $1 :: $2 }
;
