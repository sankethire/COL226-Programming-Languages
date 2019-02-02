(* Dummy implementation of A1 *)
open A0
exception Not_implemented
exception Invalid_opcode_list

type  exptree =  N of bigint
  | Plus of exptree * exptree
  | Minus of exptree * exptree
  | Mult of exptree * exptree
  | Div of exptree * exptree
  | Rem of exptree * exptree
  | Nega of exptree
  | Abs of exptree
type opcode = CONST of bigint | PLUS | TIMES | MINUS | DIV | REM | ABS | UNARYMINUS
let rec eval ex =  match ex with
  N i -> i
  |Plus(e1,e2) -> A0.add (eval e1) (eval e2)
  |Minus(e1,e2) -> A0.sub (eval e1) (eval e2)
  |Mult(e1,e2) -> A0.mult (eval e1) (eval e2)
  |Div(e1,e2) -> A0.div (eval e1) (eval e2)
  |Rem(e1,e2) -> A0.rem (eval e1) (eval e2)
  |Nega(e1) -> A0.minus (eval e1)
  |Abs(e1) -> A0.abs (eval e1)

let rec stackmc (stk: bigint list) (pgm: opcode list) = match (stk,pgm) with
  (s,[]) -> List.hd s
  |(s,(CONST(i))::opc) -> stackmc(i::s) (opc)
  |(i::s,ABS::opc) -> stackmc((A0.abs i)::s) (opc)
  |(i1::i2::s,PLUS::opc) -> stackmc((A0.add i1 i2)::s) (opc)
  |(i1::i2::s,MINUS::opc) -> stackmc((A0.sub i1 i2)::s) (opc)
  |(i1::i2::s,TIMES::opc) -> stackmc((A0.mult i1 i2)::s) (opc)
  |(i1::i2::s,DIV::opc) -> stackmc((A0.div i1 i2)::s) (opc)
  |(i1::i2::s,REM::opc) -> stackmc((A0.rem i1 i2)::s) (opc)
  |(i::s,UNARYMINUS::opc) -> stackmc((A0.minus i)::s) (opc)
  |_ -> raise Invalid_opcode_list  

let rec compile ex = match ex with
  N i -> [CONST i]
  |Abs e1 -> (compile e1) @ [ABS]
  |Plus(e1,e2) -> (compile e1) @ (compile e2) @ [PLUS]
  |Minus(e1,e2) -> (compile e1) @ (compile e2) @ [MINUS]
  |Mult(e1,e2) ->(compile e1) @ (compile e2) @ [TIMES]
  |Div(e1,e2) -> (compile e1) @ (compile e2) @ [DIV]
  |Rem(e1,e2) -> (compile e1) @ (compile e2) @ [REM]
  |Nega(e1) -> (compile e1) @ [UNARYMINUS]


(* let a = mk_big 5;;
(* val a : A0.bigint = (NonNeg, [5]) *)
let b = mk_big (-7);;
(* val b : A0.bigint = (Neg, [7]) *)
let c = mk_big 2;;
(* val c : A0.bigint = (NonNeg, [2]) *)
let d = mk_big 0;;
(* val d : A0.bigint = (NonNeg, [0]) *)

let e = (Plus(Mult(N a,N b),Mult(N c,N d)));; *)
(* let a = mk_big 5;;
let b = mk_big (-7);;
let c = mk_big 2;;
let d = mk_big 0;;
let e = (Plus(Mult(N a,N b),Mult(N c,N d)));; *) 
