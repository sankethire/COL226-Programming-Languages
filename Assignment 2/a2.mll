(* ************************************************************************************** *)
(*                     Assignment 2 :Building a scanner using OCaml-Lex                   *)
(* ************************************************************************************** *)

(* ********************** *)
(*         HEADER         *)
(* ********************** *)

{
  type token =
                INT of int          (* integer constant, positive or negative w/o leading zeros *)
              |  TRUE                (* boolean constant "T" *)
              |  FALSE               (* boolean constant "F" *)
              |  ABS                 (* unary operator, "abs" *)
              |  PLUS                (* arithmetic plus, "+" *)
              |  MINUS               (* arithmetic minus, "-" *)
              |  MUL                 (* arithmetic multiply, "*" *)
              |  DIV                 (* integer div, "div" *)
              |  MOD                 (* remainder, "mod" *)
              |  EXP                 (* exponentiation, "^" *)
              |  LP                  (* left paren, "(" *)
              |  RP                  (* right paren, ")" *)
              |  NOT                 (* boolean NOT, "not" *)
              |  AND                 (* boolean AND, "/\ " *)
              |  OR                  (* boolean OR, "\/" *)
              |  EQ                  (* equal to, "=" *)
              |  GTA                 (* greater than, ">" *)
              |  LTA                 (* less than, "<" *)
              |  GEQ                 (* greater than/equal to, ">=" *)
              |  LEQ                 (* less than/equal to, "<=" *)
              |  IF                  (* keyword "if" *)
              |  THEN                (* keyword "then" *)
              |  ELSE                (* keyword "else" *)
              |  ID of string        (* variable identifier, alphanumeric string with first char lowercase *)
              |  DEF                 (* definition construct, "def" *)
              |  DELIMITER;;         (* delimiter, ";" *)

  (* This exceptions raise if some invalid input notation is introduced
      and don't promote to read the input string further for lexing *)
  exception InvalidToken of char;;    (* raise InvalidToken exception when we encounter an invalid character *)      
  exception InvalidTokenStr of string;;  (* raise InvalidToken exception when we encounter an invalid string *)
}
(* ************************************** *)
(*               DEFINITIONS              *)
(* ************************************** *)

(* positive int can be both num , +num *)
(* whereas negative int is always -num *)
let int_ = ('-'|'+')?('0'|['1'-'9']['0'-'9']*)

let abs_ = "abs"

let add = "+"
let sub = "-"
let mul = "*"
let div_ = "div"
let mod_ = "mod"
let expo = "^"

let lparen = "("
let rparen = ")"

let true_ = "T"
let false_ = "F"

let not_ = "not"

(* Escaping backslashes in /\ and  \/ *)
let and_ = "/\\"
let or_ = "\\/"

let eq = "="
let gt = ">"
let lt = "<"
let gte = ">="
let lte = "<="

let if_ = "if"
let then_ = "then"
let else_ = "else"

let id = ['a'-'z']['a'-'z''A'-'Z''0'-'9']*

(* def should be placed before id in scanner rules to prioritize accordingly. *)
let def_ = "def"

let delimiter_ = ";"

let whitespace = [' ''\t''\n']

(* other_than_all regex includes all invalid strings possible *)
let other_than_all = [^' ''\t''\n''+''-''=''*'';''^''('')''>''<''/''\\''T''F']+

(* This is not implemented in scanner
  since in piazza post "Inconsistency in valid and invalid tokens" 
  The input corresponding to [INT(2); MINUS; INT (5)] is "2 - 5", "2- 5"
  i.e. input string such as "2-", "3+" are valid tokens.
  *)
let int_nonint_concat = ('-'|'+')?('0'|['1'-'9']['0'-'9']*)['+''-''=''*''>''<''/''\\''T''F']+[' ']
(* int_nonint_concat regex to be tested in demo *)
(* |int_nonint_concat as inc {raise (InvalidTokenStr (String.sub inc 0 (String.length inc - 1)))} *)

(* ****************************** *)
(*              RULES             *)
(* ****************************** *)

rule read = parse
  int_ as n                     {(INT (int_of_string n)) :: (read lexbuf)}
  |abs_                         {(ABS) :: (read lexbuf)}
  |add                          {(PLUS) :: (read lexbuf)}
  |sub                          {(MINUS) :: (read lexbuf)}
  |mul                          {(MUL) :: (read lexbuf)}
  |div_                         {(DIV) :: (read lexbuf)}
  |mod_                         {(MOD) :: (read lexbuf)}
  |expo                         {(EXP) :: (read lexbuf)}
  |lparen                       {(LP) :: (read lexbuf)}
  |rparen                       {(RP) :: (read lexbuf)}
  |true_                        {(TRUE) :: (read lexbuf)}
  |false_                       {(FALSE) :: (read lexbuf)}
  |not_                         {(NOT) :: (read lexbuf)}
  |or_                          {(OR) :: (read lexbuf)} 
  |and_                         {(AND) :: (read lexbuf)}
  |eq                           {(EQ) :: (read lexbuf)}
  |gt                           {(GTA) :: (read lexbuf)}
  |lt                           {(LTA) :: (read lexbuf)}
  |gte                          {(GEQ) :: (read lexbuf)}
  |lte                          {(LEQ) :: (read lexbuf)}
  |if_                          {(IF) :: (read lexbuf)}
  |then_                        {(THEN) :: (read lexbuf)}
  |else_                        {(ELSE) :: (read lexbuf)}
  |def_                         {(DEF) :: (read lexbuf)}
  |id as i                      {(ID (i)) :: (read lexbuf)}
  |delimiter_                   {(DELIMITER) :: (read lexbuf)}
  |whitespace                   {read lexbuf}             
  |_ as o                       {raise (InvalidToken o)}
  |other_than_all as ota        {raise (InvalidTokenStr ota)}
  |eof                          {[]}

(* ******************************** *)
(*              TRAILER             *)
(* ******************************** *)

{
  let scanner s = read(Lexing.from_string s)
}


(* //////////////////////////////////////////////////////////////////// *)

(* ************************************** *)
(*  Examples and their respective outputs *)
(* ************************************** *)

(* If int_nonint_concat is included in scanner then
  # scanner "5- 2";;
  Exception: InvalidTokenStr "5-".
 *)

(* 
# scanner "abc%9re";;
Exception: InvalidTokenStr "abc%9re".
# scanner "007";;
Exception: InvalidTokenStr "007".
# scanner "a @ b";;
Exception: InvalidToken '@'.


# scanner "5+2";;
- : token list = [INT 5; INT 2]
# scanner "5 + 2";;
- : token list = [INT 5; PLUS; INT 2]

(* Piazza examples *)
# scanner "+ 5";;
- : token list = [PLUS; INT 5]
# scanner "-50";;
- : token list = [INT (-50)]
# scanner "+5";;
- : token list = [INT 5]
# scanner "52-5 ";;
- : token list = [INT 52; INT (-5)]
# scanner "52 - 5 ";;
- : token list = [INT 52; MINUS; INT 5]
# scanner "5 \\/ 10";;
- : token list = [INT 5; OR; INT 10]
# scanner "a /\\ 5";;
- : token list = [ID "a"; AND; INT 5]
# scanner "5 # b";;
Exception: InvalidToken '#'.
# scanner "043";;
Exception: InvalidTokenStr "043".
# scanner "2 -5";;
- : token list = [INT 2; INT (-5)]
# scanner "2-5";;
- : token list = [INT 2; INT (-5)]
# scanner "4*3" ;;
- : token list = [INT 4; MUL; INT 3]
# scanner "3+x";;
- : token list = [INT 3; PLUS; ID "x"]
# scanner "3x";;
Exception: InvalidTokenStr "3x".
# scanner "not Not";;
Exception: InvalidTokenStr "Not".
# scanner "5+ 2";;
- : token list = [INT 5; PLUS; INT 2]
# scanner "2- 5";;
- : token list = [INT 2; MINUS; INT 5]
 *)
(* //////////////////////////////////////////////////////////////////// *)
(* *************************************************** *)
(*    Examples to run directly on OCaml Interpreter    *)
(* *************************************************** *)
(* 
  scanner "abc%9re";;
  scanner "007";;
  scanner "a @ b";;
  scanner "5+2";;
  scanner "5 + 2";;

(* Piazza examples *)
  scanner "+ 5";;
  scanner "-50";;
  scanner "+5";;
  scanner "52-5 ";;
  scanner "52 - 5 ";;
  scanner "5 \\/ 10";;
  scanner "a /\\ 5";;
  scanner "5 # b";;
  scanner "043";;
  scanner "2 -5";;
  scanner "2-5";;
  scanner "4*3" ;;
  scanner "3+x";;
  scanner "3x";;
  scanner "not Not";;
  scanner "5+ 2";;
  scanner "2- 5";;
*)

(* 
# let invalid_s = "5 * 4 + 3 - 9 <= >= < > = -99 007
           123cool sAnKeT cool123 not then else if 0
           ((5+(6-5+(4-3)))>10)     
           def ( 0000 ) wow! 
           F /\\ T = F ; F \\/ T = T ; ";;
# scanner invalid_s;;
Exception: InvalidTokenStr "007".         
 *)

(*
 # let valid_s = "5 * 4 + 3 - 9 <= >= < > = -99 
            sAnKeT cool123 not then else if 0
           ((5+(6-5+(4-3)))>10)     
           def ( 1000 )  
           F /\\ T = F ; F \\/ T = T ; ";;  
 # scanner valid_s;;
- : token list =
[INT 5; MUL; INT 4; PLUS; INT 3; MINUS; INT 9; LEQ; GEQ; LTA; GTA; EQ;
 INT (-99); ID "sAnKeT"; ID "cool123"; NOT; THEN; ELSE; IF; INT 0; LP; LP;
 INT 5; PLUS; LP; INT 6; INT (-5); PLUS; LP; INT 4; INT (-3); RP; RP; RP;
 GTA; INT 10; RP; DEF; LP; INT 1000; RP; FALSE; AND; TRUE; EQ; FALSE;
 DELIMITER; FALSE; OR; TRUE; EQ; TRUE; DELIMITER]          
 *)

(* //////////////////////////////////////////////////////////////////// *)
