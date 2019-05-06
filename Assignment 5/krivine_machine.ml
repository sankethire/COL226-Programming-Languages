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

type cls = Clos of env * expr and env = Table of (string * cls) list;;

type opcode = CLOSUREOP of env * expr
              | CONST of int
              | ADD
              | MUL 
              | AND 
              | OR 
              | BOOL of bool
              | CMP
              | IFTE 
;;

type answer = N of int
              | B of bool
;;

let rec lookuptable ltable t = match (ltable,t) with
  | (Table((a,Clos(x,y))::tail),b) -> if(a=t) then Clos(x,y) else lookuptable (Table(tail)) b 
  | (Table ([]),_) -> raise InvalidInput
;;

let rec execute clsr clsrList = match (clsr, clsrList) with
 (* ADDITION *)
 | (Clos(env,Integer(n)),(CLOSUREOP(env2,e)::ADD::s)) -> execute (Clos(env2,e)) (CONST(n)::ADD::s)
 | (Clos(env,Integer(n1)),CONST(n2)::ADD::s) -> execute (Clos(env,Integer(n1+n2))) (s)
 | (Clos(env,Plus(e1,e2)),s) -> execute (Clos(env,e1)) (CLOSUREOP(env,e2)::ADD::s)

 (* MULTIPLICATION *)
 | (Clos(env,Integer(n1)),CONST(n2)::MUL::s) -> execute (Clos(env,Integer(n1*n2))) (s)
 | (Clos(env,Integer(n)),(CLOSUREOP(env2,e)::MUL::s)) -> execute (Clos(env2,e)) (CONST(n)::MUL::s)
 | (Clos(env,Mult(e1,e2)),s) -> execute (Clos(env,e1)) (CLOSUREOP(env,e2)::MUL::s)

  (* AND *)
 | (Clos(env,Bool(b1)),BOOL(b2)::AND::s) -> execute (Clos(env,Bool(b1 && b2))) (s)
 | (Clos(env,Bool(b)),(CLOSUREOP(env2,e)::AND::s)) -> execute (Clos(env2,e)) (BOOL(b)::AND::s)
 | (Clos(env,And(e1,e2)),s) -> execute (Clos(env,e1)) (CLOSUREOP(env,e2)::AND::s)

  (* OR *)
  | (Clos(env,Bool(b1)),BOOL(b2)::OR::s) -> execute (Clos(env,Bool(b1 || b2))) (s)
  | (Clos(env,Bool(b)),(CLOSUREOP(env2,e)::OR::s)) -> execute (Clos(env2,e)) (BOOL(b)::OR::s)
  | (Clos(env,Or(e1,e2)),s) -> execute (Clos(env,e1)) (CLOSUREOP(env,e2)::OR::s) 

  (* COMPARE *)
  (* | (Clos(env,Integer(n)),(CLOSUREOP(env2,e)::CMP::s)) -> if (n > 0) then execute (Clos(env2,e)) (BOOL(true)::CMP::s) else execute (Clos(env2,e)) (BOOL(false)::CMP::s)
  | (Clos(env,Integer(n1)),CONST(n2)::CMP::s) -> if (n2 > 0) then execute (Clos(env,Bool(true))) (s) else execute (Clos(env,Bool(false))) (s)
  | (Clos(env,Cmp(e)),s) -> execute (Clos(env,e)) (CLOSUREOP(env,e)::CMP::s) *)

 (* IF THEN ELSE *)
 (* | (Clos(env,If_Then_Else(e1,e2,e3)),s) -> execute (Clos(env,e1)) (CLOSUREOP(env,e2)::CLOSUREOP(env,e3)::IFTE::s)
 | (Clos(env,Bool(true)),CLOSUREOP(env2,e2)::CLOSUREOP(env3,e3)::IFTE::s) -> execute (Clos(env2,e2)) s
 | (Clos(env,Bool(false)),CLOSUREOP(env2,e2)::CLOSUREOP(env3,e3)::IFTE::s) -> execute (Clos(env3,e3)) s *)

 | (Clos (env, If_Then_Else(e1, e2, e3)),s) ->
                          (let ans = (execute (Clos(env, e1)) []) in
                            (match ans with
                            | B b -> (if b then (execute (Clos(env,e2)) s) else (execute (Clos(env,e3)) s))
                            | _ -> raise InvalidInput))

 | (Clos(env,V(x)),s) -> execute (lookuptable env x ) (s)
 | (Clos(Table(ll),Lambda(V x,e1)),CLOSUREOP(envv,expp)::s) -> execute (Clos(Table((x,Clos(envv,expp))::ll),e1)) (s)
 | (Clos(env,App(e1,e2)),s) -> execute (Clos(env,e1)) (CLOSUREOP(env,e2)::s)
 (* Integer *)
 | (Clos(env,Integer(n1)),s) -> N(n1)
 (* Boolean *)
 | (Clos(env,Bool(true)),s) -> B (true)
 | (Clos(env,Bool(false)),s) -> B (false)
 (* Compare *)
 (* | (Clos(env,Cmp(Integer i)),s) -> if (i>0) then B(true) else B(false) *)
 | (Clos(env,Cmp(e)),s) -> (
                              match (execute (Clos(env, e)) []) with
                              | N i -> if (i>0) then B(true) else B(false)
                              | _ -> raise InvalidInput
                           )

 | (_,_) -> raise InvalidInput
;;

let krivine ex = execute (Clos(Table([]),ex)) [];;

(* let p1 =  App (Lambda (V "x", Mult (Integer 3, V "x")), Integer 4);;
let p2 = If_Then_Else
   (Cmp (Integer 7),
    App (Lambda (V "x", Plus (Integer 3, V "x")), Integer 31), 
    Integer 0);;
let p3 = If_Then_Else
    (Cmp (Integer 0),
    App (Lambda (V "x", Plus (Integer 3, V "x")), Integer 4),
        Integer 110);;

krivine (p1);;        
krivine (p2);;        
krivine (p3);;         *)


