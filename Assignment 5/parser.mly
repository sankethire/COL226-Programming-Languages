%{
  open Secd_machine
  open Krivine_machine
%}

%token <int> INT
%token <bool> BOOL
%token <string> ID
%token ADD MUL IF THEN ELSE FI CMP AND OR LP RP CMP APP LAMBDA IN EQ EOF 

%start mains maink
%type <Secd_machine.expr> mains /* Return type */
%type <Krivine_machine.expr> maink /* Return type */
%%

mains: 
  exprs EOF { $1 }
;

exprs: 
     |  subexps { $1 }
     |  exprs ADD subexps { Plus(($1),($3)) }
     |  exprs OR subexps  { Or(($1),($3)) }
     |  CMP subexps { Cmp($2) }
     |  APP LAMBDA subexps EQ subexps IN exprs {App(Lambda($3,$7),$5)}
     |  IF exprs THEN exprs ELSE exprs FI { If_Then_Else($2,$4,$6) }
;

subexps:
     |  constant1s  { $1 }
     |  subexps MUL constant1s  { Mult(($1),($3)) }
     |  subexps AND constant1s  { And(($1),($3)) }

constant1s:
     |  constant2s  { $1 }
;

constant2s:
     |  LP exprs RP { $2 }
     |  ID  { V($1) }
     |  INT { Integer($1) }
     |  BOOL  { Bool($1) }
;

maink: 
  exprk EOF { $1 }
;

exprk: 
     |  subexpk { $1 }
     |  exprk ADD subexpk { Plus(($1),($3)) }
     |  exprk OR subexpk  { Or(($1),($3)) }
     |  CMP subexpk { Cmp($2) }
     |  APP LAMBDA subexpk EQ subexpk IN exprk {App(Lambda($3,$7),$5)}
     |  IF exprk THEN exprk ELSE exprk FI { If_Then_Else($2,$4,$6) }
;

subexpk:
     |  constant1k { $1 }
     |  subexpk MUL constant1k  { Mult(($1),($3)) }
     |  subexpk AND constant1k  { And(($1),($3)) }
;

constant1k:
     |  constant2k  { $1 }
;

constant2k:
     |  LP exprk RP { $2 }
     |  ID  { V($1) }
     |  INT { Integer($1) }
     |  BOOL  { Bool($1) }
;