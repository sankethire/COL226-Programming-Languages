exception CompileError;;
exception EmptyTable;;
exception InvalidInput;;

type expr = V of string 
            | Lambda of (expr * expr) 
            | App of (expr * expr) 
            | Plus of (expr * expr) 
            | Mult of (expr * expr) 
            | And of (expr * expr) 
            | Or of (expr * expr) 
            | Bool of bool 
            | Integer of int 
            | Cmp of expr 
            | If_Then_Else of (expr * expr * expr)
;;

type opcode = VAR of string
              | CONST of int
              | ADD
              | MUL
              | AND
              | OR 
              | BOOL of bool
              | CMP
              | IFTE 
              | OPL of opcode list
              | APP
              | LAMBDA of (opcode*(opcode list))
;;


let rec compile_for_secd e = match e with
            V(s) -> [VAR s]
            | Integer(i) -> [CONST i]
            | Bool(b) -> [BOOL b]
            | Plus(e1,e2) -> (compile_for_secd e1) @ (compile_for_secd e2) @ [ADD]
            | Mult(e1,e2) ->  (compile_for_secd e1) @ (compile_for_secd e2) @ [MUL]
            | And(e1,e2) -> (compile_for_secd e1) @ (compile_for_secd e2) @ [AND]
            | Or(e1,e2) -> (compile_for_secd e1) @ (compile_for_secd e2) @ [OR]
            | Cmp(e) -> (compile_for_secd e) @ [CMP]
            | If_Then_Else(e1,e2,e3) -> (compile_for_secd e1) @ (compile_for_secd e2) @ (compile_for_secd e3) @ [IFTE]
            | Lambda(V(s),e) -> [LAMBDA (VAR s, (compile_for_secd e))]
            | App(e1,e2) -> (compile_for_secd e1) @ [OPL(compile_for_secd e2)] @ [APP]
            | _ -> raise CompileError
;;  

let rec lookuptable ltable t = match ltable with
                                [] -> raise EmptyTable
                                | (tup1,tup2)::tail -> if (tup1 = t) then tup2 else (lookuptable tail t)
;; 

type answer = N of int
              | B of bool
              | VClos of table*string*(opcode list)
and table = (string * answer) list
;;

let conv a = match a with
  B(b) -> b
  |_ -> raise InvalidInput
;;

let rec execute stack environment opc dump = match (stack, environment, opc, dump) with
              ([vr],e,[],[]) -> vr
              | ([vr],e,[],(s,e1,c)::d) -> execute (vr::s) e1 c d
              | (s,e,(CONST i)::c,d) -> execute ((N i)::s) e c d
              | (s,e,(BOOL b)::c,d) -> execute ((B b)::s) e c d
              | ((N i1)::(N i2)::s,e,ADD::c,d) -> execute (N (i1+i2)::s) e c d
              | ((N i1)::(N i2)::s,e,MUL::c,d) -> execute (N (i1*i2)::s) e c d                                
              | ((B b1)::(B b2)::s,e,AND::c,d) -> execute (B (b1 && b2)::s) e c d
              | ((B b1)::(B b2)::s,e,OR::c,d) -> execute (B (b1 || b2)::s) e c d
              | ((N i)::s,e,CMP::c,d) -> if (i > 0) then (execute ((B true)::s) e c d) else (execute ((B false)::s) e c d)
              | (a::b1::b2::s,e,IFTE::c,d) -> if (conv b2) then (execute (b1::s) e c d) else (execute (a::s) e c d)
              |(s, e, (VAR x)::c, d) -> execute ((lookuptable e x)::s) e c d
              |(s, e, (LAMBDA(VAR x, t))::c, d) -> execute ((VClos (e, x, t))::s) e c d
              |(s, e, OPL(l)::c, d) -> execute s e (l@c) d
              |(opl::VClos(e',p,t)::s, e, APP::c, d) -> execute [] ((p,opl)::e') t ((s,e,c)::d)
              | _ -> raise InvalidInput
              ;;

let secd exp = execute [] [] (compile_for_secd exp) [];;

let p1 =  App (Lambda (V "x", Mult (Integer 3, V "x")), Integer 4);;
let p2 = If_Then_Else
   (Cmp (Integer 7),
    App (Lambda (V "x", Plus (Integer 3, V "x")), Integer 31), 
    Integer 0);;
let p3 = If_Then_Else
    (Cmp (Integer 0),
    App (Lambda (V "x", Plus (Integer 3, V "x")), Integer 4),
        Integer 110);;



            