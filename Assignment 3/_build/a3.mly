%{
    open A1
%}

/*
- Tokens (token name and rules) are modified wrt to A2. Please make necessary changes in A3
- LP and RP are left and right parenthesis
- Write grammar rules to recognize
  - >= <= from GT EQ LT tokens
  - if then else fi
*/
/* Tokens are defined below.  */
%token <int> INT
%token <bool> BOOL
%token <string> ID
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ EOF
%start main
%type <A1.exptree> main /* Return type */

%left LT
%left GT
%left EQ
%left DISJ
%left CONJ
%left MINUS PLUS
%left TIMES DIV REM

%%
/*
DESIGN a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS)
The language should contain the following types of expressions:  integers and booleans.
*/

/* main:
  INT EOF   { N($1) }
; */
main:
  expr EOF  { $1 }
;

expr:
    | subexp                              { $1 }
    | expr PLUS subexp                    { Add($1,$3) } /* Created a tree with PLUS at root and two subtrees corresponding to left: add_expression and right: mult_expression */
    | expr MINUS subexp                   { Sub($1,$3) }
    | expr DISJ subexp                    { Disjunction($1,$3) }
    | expr LT subexp                      { LessT($1,$3) }
    | expr GT subexp                      { GreaterT($1,$3) }
    | expr EQ subexp                      { Equals($1,$3) }
    | IF expr THEN subexp ELSE subexp FI  { IfThenElse($2,$4,$6)}
    | expr COMMA subexp                   { GreaterTE($1,$3) }
    | expr PROJ subexp                    { LessTE($1,$3) }
;



subexp:
    | constant1                           { $1 }
    | subexp TIMES constant1              { Mult($1,$3) }
    | subexp CONJ constant1               { Conjunction($1,$3) }
    | subexp DIV constant1                { Div($1,$3) }
    | subexp REM constant1                { Rem($1,$3) }
;    

constant1:
    | constant2                           { $1 }
    | ABS constant1                       { Abs($2) }
    | NOT constant1                       { Not($2) }
    | TILDA constant1                     { Negative($2) }

constant2:
    | LP expr RP                          { $2 }
    | ID                                  { Var($1) }      /* To be interpreted as a variable name with string as tokenised */
    | INT                                 { N($1) }      /* To be interpreted as an integer with its value as tokenised   */
    | BOOL                                { B($1)}
;