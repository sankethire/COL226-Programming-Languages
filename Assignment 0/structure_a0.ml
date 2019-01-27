(* ************************************************************************************** *)
(*                                Assignment 0 : BIGNUM PACKAGE                           *)
(* ************************************************************************************** *)
open Signature_a0
	module A0 : BigInt = struct
	(* Definng type of BigInt as positive or negative bigint *)
	type bigint = sign * int list
	and sign = Neg | NonNeg;;
	(* Exception to handle the case of dividing by in quotient and remainder calculation *)
	exception Division_by_zero;;

	(* Function which converts int to int list *)
	let int_to_intlist n =
		let rec intl acc n = match n with
			x when x<10 -> n::acc
			|_ -> intl((n mod 10)::acc) (n/10)
		in intl [] n 

	(* Function which removes leading zeroes of int list *)
	let rec remove0 l = match l with
				[] -> [0]
				|x::xs -> if x = 0 then remove0 xs else l

	(* Function which removes leading zeroes from a bigint's int list *)
	let remove0_bigint b:bigint = match b with (x,l) -> (x,remove0 l)

	(* Function which converts int list to string *)
	let rec intlist_to_string l = match l with
		[] -> ""
		|x::xs -> string_of_int x ^ (intlist_to_string xs)

	(******************************************************************************************)
	(*                         mk_big and print_num function                                  *)
	(******************************************************************************************)
	(* Function which converts int to bigint  *)
	let mk_big n = match n with
		0 -> (NonNeg, [0])
		|x -> if x > 0 then (NonNeg, int_to_intlist x) else (Neg, int_to_intlist (-1*x))

	(* prints bigint in the form of string *)
	let rec print_num (b_int:bigint) = match remove0_bigint b_int with
			(NonNeg, [0]) -> "0"
			|(Neg,[0]) -> "0"
			|(NonNeg,x) -> intlist_to_string (remove0 x)
			|(Neg,x) -> "-"^(intlist_to_string (remove0 x))    

	(******************************************************************************************)
	(*                     IMPLEMENTATION OF LIST ADDITION WITH CARRY                         *)
	(******************************************************************************************)
	let rec list_addition l1 l2 carry = match (l1, l2, carry) with
					| l1, [], 0       -> l1
					| [], l2, 0       -> l2
					| l1, [], carry   -> list_addition l1 [carry] 0
					| [], l2, carry   -> list_addition [carry] l2 0
					| l1, l2, carry ->
						let addition = (List.hd l1) + (List.hd l2) + carry
						in  addition mod 10 :: list_addition (List.tl l1) (List.tl l2) (addition / 10)

	let  list_add l1 l2 = List.rev (list_addition (List.rev l1 ) (List.rev l2) 0)

	(* ****************************************************************************************)
	(*                    IMPLEMENTATION OF LIST SUBTRACTION WITH CARRY                       *)
	(* ************************************************************************************** *)
	let rec list_subtraction l1 l2 carry = match (l1, l2, carry) with
					| [], [], 0         -> []
					| l1, [], 0      -> l1
					| l1, [], carry  -> list_subtraction l1 [carry] 0
					| l1, l2, carry ->
							let subtraction = (List.hd l1) - (List.hd l2) - carry in
									if (subtraction >= 0)
									then subtraction :: list_subtraction (List.tl l1) (List.tl l2) 0
									else subtraction + 10 :: list_subtraction (List.tl l1) (List.tl l2) 1

	let subtract_list l1 l2 = List.rev (list_subtraction (List.rev l1 ) (List.rev l2) 0)

	(* ****************************************************************************************)
	(*                    IMPLEMENTATION OF LIST MULTPLICATION WITH CARRY                     *)
	(* ****************************************************************************************)
	let rec mult_by_digit_func digit carry l = function 
		[] -> remove0 (carry::l)
		|x::xs -> mult_by_digit_func digit ((x*digit+carry)/10) (((x*digit+carry) mod 10)::l) xs

	let mult_by_digit digit l = mult_by_digit_func digit 0 [] (List.rev l)  
		
	let rec mul_func l l1 = function
		[] -> l
		|x::xs -> mul_func (list_add (mult_by_digit 10 l) (mult_by_digit x l1)) l1 xs 

	let mul l1 l2 = mul_func [] l1 l2  

	(* ****************************************************************************************)
	(*                       IMPLEMENTATION OF QUOTIENT AND REMAINDER                         *)
	(* ****************************************************************************************)
	let first_tup ((x:int list),(y:int list)) = x
	let second_tup ((x:int list),(y:int list)) = y

	let lessthan l1 l2 = 
		if List.length (remove0 l1) < List.length (remove0 l2) then true
		else if List.length (remove0 l1) > List.length (remove0 l2) then false
		else (remove0 l1) < (remove0 l2)  

	let lessthaneq l1 l2 = if (remove0 l1) = (remove0 l2) then true else lessthan (remove0 l1) (remove0 l2)

	let rec quotient_multiple_func l1 l2 m = if (lessthaneq (mult_by_digit m l2) l1) then m else quotient_multiple_func (l1) (l2) (m-1)
		
	let quotient_multiple l1 l2 = quotient_multiple_func l1 l2 9

	let rec divide (l1:int list) (l2:int list) = match l2 with
		[0] -> raise Division_by_zero
		|x when List.length l1 < List.length l2 -> ([0],l1)
		|x when List.length l1 > List.length l2 -> 
																							(
																								let quotient = first_tup(divide l1 (mult_by_digit 10 l2)) in 
																								let dividend = second_tup(divide l1 (mult_by_digit 10 l2)) in
																								let quotient_digit = (quotient_multiple (dividend) (l2)) in
																								let remainder = (subtract_list dividend (mult_by_digit quotient_digit l2))  in
																								(quotient_digit::quotient, remove0 remainder )
																							)
		|x -> 
					(
						let quotient_digit = (quotient_multiple l1 l2) in
						let remainder = subtract_list l1 (mult_by_digit quotient_digit l2) in
						(quotient_digit::[], remove0 remainder)
					)

	let list_divide_quo l1 l2 = match (remove0 l1, remove0 l2) with
		(x,y) -> (remove0 (List.rev (first_tup (divide x y))))

	let list_divide_rem l1 l2 = match (remove0 l1, remove0 l2) with
		(x,y) -> (remove0 ((second_tup (divide x y))))  

	(* ****************************************************************************************)
	(*                        COMPARISON OPERATIONS ON BIGINT                                 *)
	(* ****************************************************************************************)
	(* EQUAL *)
	let eq (b_int1:bigint) (b_int2:bigint) = match (remove0_bigint b_int1, remove0_bigint b_int2) with
		((bs1,bl1),(bs2,bl2)) -> ((bs1 =bs2) && (bl1=bl2))

	(* GREATER THAN *)
	let gt (b_int1:bigint) (b_int2:bigint) = match (remove0_bigint b_int1, remove0_bigint b_int2) with
		((Neg,x),(NonNeg,y)) -> false
		|((NonNeg,x),(Neg,y)) -> true
		|((NonNeg,x),(NonNeg,y)) -> lessthan y x
		|((Neg,x),(Neg,y)) -> lessthan x y  

	(* LESS THAN *)
	let lt (b_int1:bigint) (b_int2:bigint) = gt b_int2 b_int1

	(* GREATER THAN EQUAL TO *)
	let geq (b_int1:bigint) (b_int2:bigint) = (gt b_int1 b_int2) || (eq b_int1 b_int2)

	(* LESS THAN EQUAL TO *)
	let leq (b_int1:bigint) (b_int2:bigint) = (lt b_int1 b_int2) || (eq b_int1 b_int2)

	(******************************************************************************************)
	(*                        ARITHMETIC OPERATIONS ON BIGINT                                 *)
	(******************************************************************************************)
	(* ADDITION *)
	let add (b_int1:bigint) (b_int2:bigint) = match (remove0_bigint b_int1, remove0_bigint b_int2) with
		((NonNeg,x),(NonNeg,y)) -> remove0_bigint (NonNeg, list_add x y)
		|((Neg,x),(Neg,y)) -> remove0_bigint (Neg, list_add x y)
		|((NonNeg,x),(Neg,y)) -> 
														(
															match lessthan x y with
															true -> remove0_bigint (Neg,subtract_list y x)
															|false -> remove0_bigint (NonNeg, subtract_list x y)
														)
		|((Neg,x),(NonNeg,y)) -> 
														(
															match lessthan y x with
															true -> remove0_bigint (Neg,subtract_list x y)
															|false -> remove0_bigint (NonNeg, subtract_list y x)
														)   


	(* MINUS OR UNARY NEGATION*)
	let minus (b_int:bigint) = match b_int with
		(NonNeg,[0]) -> (NonNeg,[0])                         
		|(Neg,[0]) -> (NonNeg,[0])
		|(NonNeg,x) -> (Neg,x)
		|(Neg,x) -> (NonNeg,x)

	(* SUBTRACTION *)
	let sub (b_int1:bigint) (b_int2:bigint) = remove0_bigint (add b_int1 (minus b_int2))

	(* MULTIPLICATION *)
	let mult (b_int1:bigint) (b_int2:bigint) = match (remove0_bigint b_int1, remove0_bigint b_int2) with
		((NonNeg,x),(NonNeg,y)) -> remove0_bigint (NonNeg,mul x y)
		|((Neg,x),(Neg,y)) -> remove0_bigint (NonNeg,mul x y)
		|((NonNeg,x),(Neg,y)) -> remove0_bigint (Neg,mul x y)
		|((Neg,x),(NonNeg,y)) -> remove0_bigint (Neg,mul x y)

	(* QUOTIENT *)
	let div (b_int1:bigint) (b_int2:bigint) = match (remove0_bigint b_int1, remove0_bigint b_int2) with
		((NonNeg,x),(NonNeg,y)) -> remove0_bigint (NonNeg,list_divide_quo x y)
		|((Neg,x),(Neg,y)) -> remove0_bigint (NonNeg,list_divide_quo x y)
		|((NonNeg,x),(Neg,y)) -> remove0_bigint (Neg,list_divide_quo x y)
		|((Neg,x),(NonNeg,y)) -> remove0_bigint (Neg,list_divide_quo x y)

	(* REMAINDER *)
	let rem (b_int1:bigint) (b_int2:bigint) = match (remove0_bigint b_int1, remove0_bigint b_int2) with
		((NonNeg,x),(_,y)) -> remove0_bigint (NonNeg, list_divide_rem x y)
		|((Neg,x),(_,y)) -> remove0_bigint (Neg, list_divide_rem x y)  

	(* ABSOLUTE VALUE *)
	let abs (b_int:bigint) = match (remove0_bigint b_int) with
		(NonNeg,x) -> (NonNeg,x)
		|(Neg,x) -> (NonNeg,x)
end