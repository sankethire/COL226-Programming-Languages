#directory "_build";;
#load "krivine_machine.cmo";;
#load "secd_machine.cmo";;
#load "lexer.cmo";;
#load "parser.cmo";;
open Krivine_machine;;
open Secd_machine;;
open Lexer;;
open Parser;;
let sparser s = Parser.mains Lexer.read (Lexing.from_string s);;
let kparser s = Parser.maink Lexer.read (Lexing.from_string s);;
exception Not_implemented;;
exception InvalidTestCase;;

let p1 =  App (Lambda (V "x", Mult (Integer 3, V "x")), Integer 4);;
	(*12*)
let p2 = If_Then_Else
   (Cmp (Integer 7),
    App (Lambda (V "x", Plus (Integer 3, V "x")), Integer 31), 
    Integer 0);;
   (*34*)
let p3 = If_Then_Else
    (Cmp (Integer 0),
    App (Lambda (V "x", Plus (Integer 3, V "x")), Integer 4),
        Integer 110);;
    (*110*)

let p4 = App(Lambda(V "x", App(Lambda(V "y", And(V "x", V "y")), Bool true)), Bool false);;
(*false*)

let p5 = App(Lambda(V "x", App(Lambda(V "y", Or(V "x", V "y")), Bool true)), Bool false);;
(*true*)

let p6 = App(Lambda(V "x", Mult(V "x", App(Lambda(V "x", App(Lambda(V "y", Plus(V "x", V "y")), Integer 4)), Integer 3))), Integer 2);;
(*14*)

(* let p7 = If_Then_Else(Cmp(App(Lambda(V "x", App(Lambda(V "y", Plus(V "x", V "y")), Integer 4)), Integer (-5))), Integer (-29), App(Lambda(V "x", Plus(V "x", 
  App(Lambda(V "x", Plus(V "x", Integer 1)), Integer 7)), Integer 5)));; *)
  let p7 = If_Then_Else(
    Cmp(App(Lambda(V "x", App(Lambda(V "y", Plus(V "x", V "y")), Integer 4)), Integer (-5))), 
    Integer (-29), 
    App(Lambda(V "x", Plus(V "x", App(Lambda(V "x", Plus(V "x", Integer 1)), Integer 7))), Integer 5)
    );;  
(*13*)

let p8 = App(Lambda(V "x", App(Lambda(V "y", Plus(V "x", V "y")), Integer 4)), App(Lambda(V "x", Mult(V "x", Integer 2)), Integer 3));;
(*10*)

let p9 = App(Lambda(V "x", App(Lambda(V "y", Mult(V "x", V "y")), V "x")), Integer 4);;
(*16*)

let p10 = App(Lambda(V "x", Plus(V "x", App(Lambda(V "x", Mult(V "x", Integer 2)), App(Lambda(V "x", Plus(V "x", Integer 4)), Integer 3)))), Integer 20);;
(*34*)

let p11 = App(Lambda(V "x", App(Lambda(V "y", And(V "x", V "y")), V "x")), Bool true);;
(*true*)

let p12 = If_Then_Else(Cmp(App(Lambda(V "x", Mult(V "x", Integer 2)), Integer 4)), App(Lambda(V "x", App(Lambda(V "y", Or(V "x", V "y")), V "x")), Bool false), Bool true);;
(*false*)

let p13 = App(Lambda(V "x", And(V "x", App(Lambda(V "x", And(V "x", Bool true)), App(Lambda(V "x", And(V "x", Bool true)), Bool true)))), Bool true);;
(*true*)

let p14 = App(Lambda(V "x", And(V "x", App(Lambda(V "x", And(V "x", Bool true)), App(Lambda(V "x", And(V "x", Bool true)), Bool true)))), Bool false);;
(*false*)

let p15 = If_Then_Else(Cmp(App(Lambda(V "x", Mult(V "x", App(Lambda(V "y", V "y"), V "x"))), Integer 1)), App(Lambda(V "x", Plus(V "x", App(Lambda(V "x", Plus(V "x", Integer 1)), Integer 3))), Integer 5), Integer (-1));;
(*9*)


(*Your code will go here*)
(*For thise who have implemented lexer parser, modify the testcases in your grammar and you will have to get those tet_cases at the time of the demo*)

let eval_secd inp = match (secd inp) with
                    N i -> Integer i
                    | B b -> Bool b    
                    | _ -> raise InvalidTestCase            
;;

let eval_krivine inp = match (krivine inp) with
                    N i -> kparser(string_of_int(i))
                    | B b -> if b then kparser("T") else kparser("F")             
                    | _ -> raise InvalidTestCase   
;;

(*Your code ends*)

let check_secd n inp out = let ans = eval_secd inp in
  print_string("T" ^ string_of_int(n) ^ " : ");
  try if (ans = out) 
    then print_string("Passed ") 
    else print_string("Failed ");
  with e -> print_endline("Failed : Wrong exception raised : " ^ (Printexc.to_string e))
;;

let check_krivine n inp out = let ans = eval_krivine inp in
  print_string("T" ^ string_of_int(n) ^ " : ");
  try if (ans = out) 
    then print_string("Passed ") 
    else print_string("Failed ");
  with e -> print_endline("Failed : Wrong exception raised : " ^ (Printexc.to_string e))
;;

let print_heading a = print_endline("\n" ^ a ^ " :");;

(* AST conversion to grammer in krivine *)
let k1 = kparser "apply lambda X = 4 in 3*X";;
let k2 = kparser "if (comp 7) then (apply lambda X = 31 in 3+X) else 0 fi";;
let k3 = kparser "if (comp 0) then (apply lambda X = 4 in 3+X) else 110 fi";;
let k4 = kparser "apply lambda X = F in (apply lambda Y = T in (X /\\ Y))";;
let k5 = kparser "apply lambda X = F in (apply lambda Y = T in (X \\/ Y))";;
let k6 = kparser "apply lambda X = 2 in (X * (apply lambda X = 3 in (apply lambda Y = 4 in X+Y)))";;
let k7 = kparser "if (comp (apply lambda X = -5 in (apply lambda Y = 4 in X+Y))) then -29 else (apply lambda X = 5 in (X + (apply lambda X = 7 in X+1))) fi";;
let k8 = kparser "apply lambda X = (apply lambda X = 3 in X*2) in (apply lambda Y = 4 in X+Y)";;
let k9 = kparser "apply lambda X = 4 in (apply lambda Y = X in X*Y)";;
let k10 = kparser "apply lambda X = 20 in X + (apply lambda X = (apply lambda X = 3 in X+4) in X*2)";;
let k11 = kparser "apply lambda X = T in (apply lambda Y = X in (X /\\ Y))";;
let k12 = kparser "if (comp (apply lambda X = 4 in X*2) ) then (apply lambda X = F in (apply lambda Y = X in (X \\/ Y))) else T fi";;
let k13 = kparser "apply lambda X = T in (X /\\ (apply lambda X = (apply lambda X = T in (X /\\ T)) in (X /\\ T)))";;
let k14 = kparser "apply lambda X = F in (X /\\ (apply lambda X = (apply lambda X = T in (X /\\ T)) in (X /\\ T)))";;
let k15 = kparser "if (comp (apply lambda X = 1 in (X * (apply lambda Y = X in Y)))) then (apply lambda X = 5 in (X + (apply lambda X = 3 in X+1))) else -1 fi";;

(* AST conversion to grammer in secd *)
let s1 = sparser "apply lambda X = 4 in 3*X";;
let s2 = sparser "if (comp 7) then (apply lambda X = 31 in 3+X) else 0 fi";;
let s3 = sparser "if (comp 0) then (apply lambda X = 4 in 3+X) else 110 fi";;
let s4 = sparser "apply lambda X = F in (apply lambda Y = T in (X /\\ Y))";;
let s5 = sparser "apply lambda X = F in (apply lambda Y = T in (X \\/ Y))";;
let s6 = sparser "apply lambda X = 2 in (X * (apply lambda X = 3 in (apply lambda Y = 4 in X+Y)))";;
let s7 = sparser "if (comp (apply lambda X = -5 in (apply lambda Y = 4 in X+Y))) then -29 else (apply lambda X = 5 in (X + (apply lambda X = 7 in X+1))) fi";;
let s8 = sparser "apply lambda X = (apply lambda X = 3 in X*2) in (apply lambda Y = 4 in X+Y)";;
let s9 = sparser "apply lambda X = 4 in (apply lambda Y = X in X*Y)";;
let s10 = sparser "apply lambda X = 20 in X + (apply lambda X = (apply lambda X = 3 in X+4) in X*2)";;
let s11 = sparser "apply lambda X = T in (apply lambda Y = X in (X /\\ Y))";;
let s12 = sparser "if (comp (apply lambda X = 4 in X*2) ) then (apply lambda X = F in (apply lambda Y = X in (X \\/ Y))) else T fi";;
let s13 = sparser "apply lambda X = T in (X /\\ (apply lambda X = (apply lambda X = T in (X /\\ T)) in (X /\\ T)))";;
let s14 = sparser "apply lambda X = F in (X /\\ (apply lambda X = (apply lambda X = T in (X /\\ T)) in (X /\\ T)))";;
let s15 = sparser "if (comp (apply lambda X = 1 in (X * (apply lambda Y = X in Y)))) then (apply lambda X = 5 in (X + (apply lambda X = 3 in X+1))) else -1 fi";;



(*SECD*)
print_heading "SECD test cases\n";;

check_secd 1 (eval_secd s1) (Integer 12);;
check_secd 2 (eval_secd s2) (Integer 34);;
check_secd 3 (eval_secd s3) (Integer 110);;
check_secd 4 (eval_secd s4) (Bool false);;
check_secd 5 (eval_secd s5) (Bool true);;
check_secd 6 (eval_secd s6) (Integer 14);;
check_secd 7 (eval_secd s7) (Integer 13);;
check_secd 8 (eval_secd s8) (Integer 10);;
check_secd 9 (eval_secd s9) (Integer 16);;
check_secd 10 (eval_secd s10) (Integer 34);;
check_secd 11 (eval_secd s11) (Bool true);;
check_secd 12 (eval_secd s12) (Bool false);;
check_secd 13 (eval_secd s13) (Bool true);;
check_secd 14 (eval_secd s14) (Bool false);;
check_secd 15 (eval_secd s15) (Integer 9);; 

print_heading "Krivine test cases";;

check_krivine 1 (eval_krivine k1) (Integer 12);;
check_krivine 2 (eval_krivine k2) (Integer 34);;
check_krivine 3 (eval_krivine k3) (Integer 110);;
check_krivine 4 (eval_krivine k4) (Bool false);;
check_krivine 5 (eval_krivine k5) (Bool true);;
check_krivine 6 (eval_krivine k6) (Integer 14);;
check_krivine 7 (eval_krivine k7) (Integer 13);;
check_krivine 8 (eval_krivine k8) (Integer 10);;
check_krivine 9 (eval_krivine k9) (Integer 16);;
check_krivine 10 (eval_krivine k10) (Integer 34);;
check_krivine 11 (eval_krivine k11) (Bool true);;
check_krivine 12 (eval_krivine k12) (Bool false);;
check_krivine 13 (eval_krivine k13) (Bool true);;
check_krivine 14 (eval_krivine k14) (Bool false);;
check_krivine 15 (eval_krivine k15) (Integer 9);; 
(*Krivine*)

print_string("\n");;

