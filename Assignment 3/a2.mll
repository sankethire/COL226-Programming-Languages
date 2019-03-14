{
  open A3
  exception Not_implemented
  exception InvalidToken of char;;    (* raise InvalidToken exception when we encounter an invalid character *)      
  exception InvalidTokenStr of string;;  (* raise InvalidToken exception when we encounter an invalid string *)

}

(*
  Below is a dummy implementation. Please note the following
  - Tokens are defined in A3.mly
  - Return type is token and not token list
  - End of buffer is indicated by EOF token below
  - There is no trailer. The scanner function is written in the wrapper file (test_a3.ml)
*)

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

let proj = "<="

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
let fi = "fi"

let id = ['A'-'Z']['a'-'z''A'-'Z''0'-'9''_''\'']*

(* def should be placed before id in scanner rules to prioritize accordingly. *)
let def_ = "def"

let delimiter_ = ";"

let whitespace = [' ''\t''\n']
let tilda = "~"
let comma = ">="

(* other_than_all regex includes all invalid strings possible *)
let other_than_all = [^' ''\t''\n''+''-''=''*'';''('')''>''<''/''\\''T''F']+


(* rule read = parse
   eof                { EOF }
   | ['0'-'9']+ as n  { INT (int_of_string n) }
   | _                { raise Not_implemented } *)

 rule read = parse
  int_ as n                    {(INT (int_of_string n)) }
  |tilda                        {(TILDA)}
  |abs_                         {(ABS) }
  |add                          {(PLUS) }
  |sub                          {(MINUS) }
  |mul                          {(TIMES) }
  |div_                         {(DIV) }
  |mod_                         {(REM) }
  |lparen                       {(LP) }
  |rparen                       {(RP) }
  |true_                        {BOOL(true) }
  |false_                       {BOOL(false) }
  |not_                         {(NOT) }
  |comma                        {(COMMA)}
  |proj                         {(PROJ)}
  |or_                          {(DISJ) } 
  |and_                         {(CONJ) }
  |eq                           {(EQ) }
  |gt                           {(GT) }
  |lt                           {(LT) }
  |if_                          {(IF) }
  |then_                        {(THEN) }
  |else_                        {(ELSE) }
  |fi                           {(FI)}
  |id as i                      {(ID (i)) }
  |whitespace                   {read lexbuf}             
  |eof                          {EOF} 
  |_                            {read lexbuf}
