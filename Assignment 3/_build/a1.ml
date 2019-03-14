(* Dummy implementation of A1 *)
open A0
exception Not_implemented
exception Invalid_input
exception Division_by_zero

(* abstract syntax *)
type  exptree =  
  Var of string (* variables starting with a Capital letter, represented as alphanumeric strings with underscores (_) and apostrophes (') *)
  | N of int      (* Integer constant *)
  | B of bool     (* Boolean constant *)
  (* unary operators on integers *)
  | Abs of exptree                   (* abs *)
  | Negative of exptree              (* unary minus ~ *)
  (* unary operators on booleans *)
  | Not of exptree
  (* binary operators on integers *)
  | Add of exptree * exptree         (* Addition + *)
  | Sub of exptree * exptree         (* Subtraction - *)
  | Mult of exptree * exptree        (* Multiplication * *)
  | Div of exptree * exptree         (* div *)
  | Rem of exptree * exptree         (* mod *)
  (* binary operators on booleans *)
  | Conjunction of exptree * exptree (* conjunction /\ *)
  | Disjunction of exptree * exptree (* binary operators on booleans \/ *)
  (* comparison operations on integers *)
  | Equals of exptree * exptree      (* = *)
  | GreaterTE of exptree * exptree   (* >= *)
  | LessTE of exptree * exptree      (* <= *)
  | GreaterT of exptree * exptree    (* > *)
  | LessT of exptree * exptree       (* < *)
  (* expressions using parenthesis *)
  | InParen of exptree               (* ( ) *)
  (* a conditional expression *)
  | IfThenElse of exptree * exptree * exptree (* if then else fi  *)
  (* creating n-tuples (n >= 0) *)
  | Tuple of int * (exptree list)
  (* projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n) *)
  | Project of (int*int) * exptree   (* Proj((i,n), e)  0 < i <= n *)

(* opcodes of the stack machine (in the same sequence as above) *)
type opcode = VAR of string | NCONST of bigint | BCONST of bool | ABS | UNARYMINUS | NOT
  | PLUS | MINUS | MULT | DIV | REM | CONJ | DISJ | EQS | GTE | LTE | GT | LT
  | PAREN | IFTE | TUPLE of int | PROJ of int*int

(* The type of value returned by the definitional interpreter. *)
type value = NumVal of int | BoolVal of bool | TupVal of int * (value list)

(* The language should contain the following types of expressions:  integers and booleans *)
type answer = Num of bigint | Bool of bool | Tup of int * (answer list) 

(* let rec eval ex rho = raise Not_implemented *)
(* let stackmc stk binding pgm = raise Not_implemented *)
(* let compile ex = raise Not_implemented *)


let value_to_bool v = match v with
  BoolVal(b) -> b
  | _ -> raise Invalid_input

let value_to_int v = match v with
    NumVal(i) -> i
  | _ -> raise Invalid_input 

let tup_to_list tv = match tv with
    Tuple(i,el) -> el  
    | _ -> raise Invalid_input

let rec eval ex rho = match ex with
    Var(s) -> (rho s)
  | N(i) -> NumVal(i)
  | B(b) -> BoolVal(b)

  | Abs(e) -> (
                match value_to_int(eval e rho) with
                    x -> if(x >= 0) then NumVal(x) else NumVal(-1*x)
              )
  | Negative(e) -> NumVal(-1 * value_to_int(eval e rho))

  | Not(e) -> (
              match value_to_bool(eval e rho) with
                  true -> BoolVal(false)
                | false -> BoolVal(true)
              )

  | Add(e1,e2) -> NumVal(value_to_int(eval e1 rho) + value_to_int(eval e2 rho))          
  | Sub(e1,e2) -> NumVal(value_to_int(eval e1 rho) - value_to_int(eval e2 rho))          
  | Mult(e1,e2) -> NumVal(value_to_int(eval e1 rho) * value_to_int(eval e2 rho))          
  | Div(e1,e2) -> if (value_to_int(eval e2 rho) = 0) then raise Division_by_zero else NumVal(value_to_int(eval e1 rho) / value_to_int(eval e2 rho))          
  | Rem(e1,e2) -> if (value_to_int(eval e2 rho) = 0) then raise Division_by_zero else NumVal(value_to_int(eval e1 rho) mod value_to_int(eval e2 rho))
  
  | Conjunction(e1,e2) -> BoolVal(value_to_bool(eval e1 rho) && value_to_bool(eval e2 rho))
  | Disjunction(e1,e2) -> BoolVal(value_to_bool(eval e1 rho) || value_to_bool(eval e2 rho))

  | Equals(e1,e2) -> BoolVal(value_to_int(eval e1 rho) = value_to_int(eval e2 rho))
  | GreaterTE(e1,e2) -> BoolVal(value_to_int(eval e1 rho) >= value_to_int(eval e2 rho))
  | LessTE(e1,e2) -> BoolVal(value_to_int(eval e1 rho) <= value_to_int(eval e2 rho))
  | GreaterT(e1,e2) -> BoolVal(value_to_int(eval e1 rho) > value_to_int(eval e2 rho))
  | LessT(e1,e2) -> BoolVal(value_to_int(eval e1 rho) < value_to_int(eval e2 rho))

  | InParen(e) -> eval e rho

  | IfThenElse(e1,e2,e3) -> if (value_to_bool(eval e1 rho)) then (eval e2 rho) else (eval e3 rho)

  | Tuple(i,el) -> (
                      let l = ref [] and
                      expl = Array.of_list el in
                      for p = 0 to (i-1) do
                        (
                          l := !l @ [eval expl.(p) rho]
                        )
                      done;  
                      TupVal(i,!l);
                  )
  
  | Project((i,n),e) -> (
                          match Array.of_list (tup_to_list(e)) with
                          x -> eval x.(i) rho
                        )


let rec reduce l e = match l with
          []->e | x::xs -> reduce xs (x @ e);;

let rec map f l =match l with
				 [] -> []
				| x::xs -> (f x)::(map f xs);;          

let rec compile ex = match ex with
    Var(s) -> [VAR s]
  | N(i) -> [NCONST (mk_big i)]
  | B(b) -> [BCONST b]
  | Abs(e) -> (compile e) @ [ABS]
  | Negative(e) -> (compile e) @ [UNARYMINUS]
  | Not(e) -> (compile e) @ [NOT]
  | Add(e1,e2) -> (compile e1) @ (compile e2) @ [PLUS]
  | Sub(e1,e2) -> (compile e1) @ (compile e2) @ [MINUS]
  | Mult(e1,e2) -> (compile e1) @ (compile e2) @ [MULT]                     
  | Div(e1,e2) -> (compile e1) @ (compile e2) @ [DIV]                     
  | Rem(e1,e2) -> (compile e1) @ (compile e2) @ [REM]                     
  | Conjunction(e1,e2) -> (compile e1) @ (compile e2) @ [CONJ]                     
  | Disjunction(e1,e2) -> (compile e1) @ (compile e2) @ [DISJ]                     
  | Equals(e1,e2) -> (compile e1) @ (compile e2) @ [EQS]                     
  | GreaterTE(e1,e2) -> (compile e1) @ (compile e2) @ [GTE]                     
  | LessTE(e1,e2) -> (compile e1) @ (compile e2) @ [LTE]                     
  | GreaterT(e1,e2) -> (compile e1) @ (compile e2) @ [GT]                     
  | LessT(e1,e2) -> (compile e1) @ (compile e2) @ [LT]                     
  | InParen(e) -> (compile e) @ [PAREN]                     
  | IfThenElse(e1,e2,e3) -> (compile e1) @ (compile e2) @ (compile e3) @ [IFTE]  
  | Tuple(i,el) -> (reduce (map compile el) []) @ [TUPLE (i)]
  | Project((i,n),e) ->  (reduce (map compile ( (tup_to_list e))) []) @ [PROJ (i,n)]
  (* | _ -> raise Invalid_input           *)

let rec gettop l n e r = if (e<n) then (gettop (List.tl l)  n (e+1) (r@[List.hd l])) else r;; 

let rec remtop l n e = if (e<n) then (remtop (List.tl l) n (e+1)) else l;;                        



let bint_int b = int_of_string(print_num b)

let rec stackmc stk binding pgm = match (stk,binding,pgm) with
    (s,binding,[]) -> List.hd stk

  | (s,binding,VAR(v)::opc) -> stackmc( binding (v)::s) binding  opc
  | (s,binding,NCONST(i)::opc) -> stackmc (Num(i)::s) binding opc
  | (s,binding,BCONST(b)::opc) -> stackmc (Bool(b)::s) binding opc
  
  | (Num(n)::s,binding,ABS::opc) -> stackmc ((Num(abs n))::s) binding opc
  | (Num(n)::s,binding,UNARYMINUS::opc) -> stackmc ((Num(minus n))::s) binding opc

  | (Bool(b)::s,binding,NOT::opc) -> if (b) then stackmc ((Bool(false))::s) binding opc else stackmc ((Bool(true))::s) binding opc

  | (Num(n1)::Num(n2)::s,binding,PLUS::opc) -> stackmc (Num(add n2 n1)::s) binding opc
  | (Num(n1)::Num(n2)::s,binding,MINUS::opc) -> stackmc (Num(sub n2 n1)::s) binding opc
  | (Num(n1)::Num(n2)::s,binding,MULT::opc) -> stackmc (Num(mult n2 n1)::s) binding opc
  | (Num(n1)::Num(n2)::s,binding,DIV::opc) -> stackmc (Num(div n2 n1)::s) binding opc
  | (Num(n1)::Num(n2)::s,binding,REM::opc) -> stackmc (Num(rem n2 n1)::s) binding opc

  | (Bool(b1)::Bool(b2)::s,binding,CONJ::opc) -> stackmc (Bool(b2 && b1)::s) binding opc
  | (Bool(b1)::Bool(b2)::s,binding,DISJ::opc) -> stackmc (Bool(b2 || b1)::s) binding opc

  | (Num(n1)::Num(n2)::s,binding,EQS::opc) -> stackmc (Bool(eq n2 n1)::s) binding opc
  | (Num(n1)::Num(n2)::s,binding,GTE::opc) -> stackmc (Bool(geq n2 n1)::s) binding opc
  | (Num(n1)::Num(n2)::s,binding,LTE::opc) -> stackmc (Bool(leq n2 n1)::s) binding opc
  | (Num(n1)::Num(n2)::s,binding,GT::opc) -> stackmc (Bool(gt n2 n1)::s) binding opc
  | (Num(n1)::Num(n2)::s,binding,LT::opc) -> stackmc (Bool(lt n2 n1)::s) binding opc

  | (s,binding,PAREN::opc) -> stackmc s binding opc
  
  | (Bool(b1)::Bool(b2)::Bool(b3)::s,binding,IFTE::opc) ->  if (b3) then stackmc(Bool(b2)::s) binding opc else stackmc(Bool(b1)::s) binding opc                    
  | (Num(b1)::Num(b2)::Bool(b3)::s,binding,IFTE::opc) ->  if (b3) then stackmc(Num(b2)::s) binding opc else stackmc(Num(b1)::s) binding opc                     
  | (Num(b1)::Bool(b2)::Bool(b3)::s,binding,IFTE::opc) ->  if (b3) then stackmc(Bool(b2)::s) binding opc else stackmc(Num(b1)::s) binding opc                     
  | (Bool(b1)::Num(b2)::Bool(b3)::s,binding,IFTE::opc) ->  if (b3) then stackmc(Num(b2)::s) binding opc else stackmc(Bool(b1)::s) binding opc 
  | (Tup(i,al1)::Tup(j,al2)::Bool(b3)::s,binding,IFTE::opc) -> if (b3) then stackmc(Tup(j,al2)::s) binding opc else stackmc(Tup(i,al1)::s) binding opc
  | (Tup(i,al1)::Bool(b2)::Bool(b3)::s,binding,IFTE::opc) -> if (b3) then stackmc(Bool(b2)::s) binding opc else stackmc(Tup(i,al1)::s) binding opc
  | (Tup(i,al1)::Num(b2)::Bool(b3)::s,binding,IFTE::opc) -> if (b3) then stackmc(Num(b2)::s) binding opc else stackmc(Tup(i,al1)::s) binding opc
  | (Bool(b1)::Tup(i,al1)::Bool(b3)::s,binding,IFTE::opc) -> if (b3) then stackmc(Tup(i,al1)::s) binding opc else stackmc(Bool(b1)::s) binding opc
  | (Num(b1)::Tup(i,al1)::Bool(b3)::s,binding,IFTE::opc) -> if (b3) then stackmc(Tup(i,al1)::s) binding opc else stackmc(Num(b1)::s) binding opc
  
  | (s,binding,TUPLE(n)::opc) -> stackmc (Tup(n,gettop s n 0 [])::(remtop s n 0)) binding opc
  | (s,binding,PROJ(i,n)::opc) -> (
                                      match Array.of_list (gettop s n 0 []) with
                                      x -> (
                                              match x.(i) with
                                                  Num(a) -> stackmc (Num(a)::(remtop s n 0)) binding opc
                                                | Bool(a) -> stackmc (Bool(a)::(remtop s n 0)) binding opc
                                                | Tup(i,al) -> stackmc (Tup(i,al)::(remtop s n 0)) binding opc
                                           )

                                  )
  | _ -> raise Invalid_input                                
  
            


(* let rhof s = match s with
     "true" -> BoolVal(true)
    | "false" -> BoolVal(false);; 
    
  let rhofl s = match s with
     "true" -> Bool(true)
    | "false" -> Bool(false) ;; 
*)



(*  
   let rhofl s = match s with
     "true" -> Bool(true)
    | "false" -> Bool(false) ;;
   let a = N 3;;
   let aa = N 3;;
   let bb = N 5;;
   let b = B true;;
   let c = Abs(N (-5));;
   let d = Negative(N (-5));;
   let f = Not(Conjunction(b, B false));;
   let g = Add(a,c);;                        
   let h = Sub(a,c);;                        
   let i = Mult(a,c);;                        
   let j = Div(a,c);;                        
   let l = Rem(a,c);;                     
   let n = Conjunction(b,Not(f));;
   let o = Disjunction(b,Not(f));;                        
   let p = Equals(a,aa);;
   let q = Equals(a,bb);;
   let r = LessT(a,bb);;
   let s = GreaterT(a,bb);;
   let t = Mult(N 3,InParen(Add(N 2, N 7)));;
   let u = IfThenElse(p,t,bb);;
   let e = Tuple(20,[a;aa;bb;b;c;d;f;g;h;i;j;l;n;o;p;q;r;s;t;u]);;
   let rhof s = match s with
     "true" -> BoolVal(true)
    | "false" -> BoolVal(false);;
   eval e rhof;; 
   stackmc [] rhofl (compile e);;
*)
    
(* 
- : A1.value =
TupVal (20,
 [NumVal 3; NumVal 3; NumVal 5; BoolVal true; NumVal 5; NumVal 5;
  BoolVal true; NumVal 8; NumVal (-2); NumVal 15; NumVal 0; NumVal 3;
  BoolVal false; BoolVal true; BoolVal true; BoolVal false; BoolVal true;
  BoolVal false; NumVal 27; NumVal 27])

*)

(* 
# let qq = Project((2,4), Tuple(4,[B true; B false; Add(N 2, N 3); Disjunction(B true, B false)]));;
val qq : A1.exptree =
  Project ((2, 4),
   Tuple (4,
    [B true; B false; Add (N 2, N 3); Disjunction (B true, B false)]))
# compile qq;;
- : A1.opcode list =
[BCONST true; BCONST false; DISJ; NCONST (NonNeg, [2]); NCONST (NonNeg, [3]);
 PLUS; BCONST false; BCONST true; PROJ (2, 4)]
*)


(*
# let jj = Project((1,3), Tuple(3, [N 1; N 2; N 3]));;
val jj : A1.exptree = Project ((1, 3), Tuple (3, [N 1; N 2; N 3]))
# let hh = Tuple(2,[jj;N 3]);;
val hh : A1.exptree =
  Tuple (2, [Project ((1, 3), Tuple (3, [N 1; N 2; N 3])); N 3])
# eval hh rhof;;
- : A1.value = TupVal (2, [NumVal 2; NumVal 3])
# 
  ;;
# stackmc [] rhofl (compile hh);;
- : A1.answer = Tup (2, [Num (NonNeg, [2]); Num (NonNeg, [3])])
*)
