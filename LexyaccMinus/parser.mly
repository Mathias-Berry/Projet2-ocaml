%{
(* --- préambule: ici du code Caml --- *)

open Expr   (* rappel: dans expr.ml: 
             type expr = Const of int | Add of expr*expr | Mull of expr*expr *)

%}
/* description des lexèmes, ceux-ci sont décrits (par vous) dans lexer.mll */

%token <int> INT       /* le lexème INT a un attribut entier */
%token <string> STR
%token PLUS TIMES MINUS
%token LET IN EGAL
%token LPAREN RPAREN
%token EOL             /* retour à la ligne */

%left PLUS MINUS  /* associativité gauche: a+b+c, c'est (a+b)+c */
%left TIMES  /* associativité gauche: a*b*c, c'est (a*b)*c */
%nonassoc UMINUS  /* un "faux token", correspondant au "-" unaire */
                  /* cf. son usage plus bas : il sert à "marquer" une règle pour lui donner la précédence maximale */

		    
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

  | LET STR EGAL expression IN expression		    { Letin(Variable $2, $4, $6) }
  | INT                                         { Const $1 }
  | STR                                         { Variable $1 }
  | LPAREN expression RPAREN                    { $2 } /* on récupère le deuxième élément */
  | expression PLUS expression                  { Add($1,$3) }
  | expression TIMES expression                 { Mul($1,$3) }
  | expression MINUS expression                 { Min($1,$3) }
  | MINUS expression %prec UMINUS               { Min(Const 0, $2) }
;



