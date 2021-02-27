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
%token LT LE GT GE AND OR NOT
%token LPAREN RPAREN
%token FUN TO
%token EOL             /* retour à la ligne */


%left NOT PRINT ELSE IN TO

%left PLUS MINUS  /* associativité gauche: a+b+c, c'est (a+b)+c */
%left TIMES  DIV/* associativité gauche: a*b*c, c'est (a*b)*c */

%left LE GE AND OR EGAL GT

%nonassoc FUNPRE

%nonassoc UMINUS  /* un "faux token", correspondant au "-" unaire */
                  /* cf. son usage plus bas : il sert à "marquer" une règle pour lui donner la précédence maximale */

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
  | LET STR EGAL expression IN expression		      { Letin(Variable $2,$4,$6) }
  | FUN STR TO expression													{ Fonction(Variable $2, $4) }
  | expression PLUS expression                    { Add($1,$3) }
  | expression TIMES expression                   { Mul($1,$3) }
  | expression DIV expression										  { Div($1, $3) }
  | expression MINUS expression                   { Min($1,$3) }
  | MINUS expression %prec UMINUS 								{ Min(Const 0, $2) }
  | expression LE expression                      { Le($1,$3) }
	| expression GE expression                      { Ge($1,$3) }
  | expression AND expression                     { And($1,$3) }
  | expression OR expression                      { Or($1,$3) }
  | expression EGAL expression                    { Eg($1,$3) }
  | expression GT expression                      { Gt($1,$3) }
  | NOT expression                                { Non($2) }
  | PRINT expression                              { Print($2) }
	| expression atomique %prec FUNPRE							{ Appli($1, $2) }
;

	atomique:
	| LPAREN expression RPAREN											{ $2 }
	| INT 																					{ Const $1 }
	| STR 																					{ Variable $1 }
;
