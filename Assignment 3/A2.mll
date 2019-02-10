{
	type token = Int of int | Unary_Op of string
							| Binary_Op of string
							| Parentheses of string
							| Bool_Const of string
							| Unary_Bool of string
							| Binary_Bool of string
							| Comparison_op of string
							| Conditional_op of string
							| Id of string
							| Def_constr of string 
							| Delimiter of string
              | Invalid of string
              | Invalidc of char
}

let int_ = '-'?('0'|['1'-'9']['0'-'9']*)
(* let intg = [int_] *)
let abs_ = "abs"

let add = "+"
let sub = "-"
let mul = "*"
let div_ = "div"
let mod_ = "mod"
let expo = "^"
(* let binary_op = (add|sub|mul|div_|mod_|expo) *)

let lparen = "("
let rparen = ")"
(* let paren = (lparen|rparen) *)

let true_ = "T"
let false_ = "F"
(* let bool_const = (true_|false_)   *)

let not_ = "not"

let and_ = "/\\"
let or_ = "\\/"
(* let binary_bool = (and_|or_) *)

let eq = "="
let gt = ">"
let lt = "<"
let gte = ">="
let lte = "<="
(* let comparison = (eq|gt|lt|gte|lte) *)

let if_ = "if"
let then_ = "then"
let else_ = "else"
(* let condition = (if_|then_|else_) *)

let id = ['a'-'z']['a'-'z''A'-'Z''0'-'9']*

let def_ = "def"

let delimiter_ = ";"

let whitespace = [' ''\t''\n']

let other_than_all = [^' ''\t''\n''+''-''=''*'';''^''('')''>''<''/''\\''T''F']+

rule read = parse
	int_ as n 						{(Int (int_of_string (n))) :: (read lexbuf)}
	| abs_  							{(Unary_Op "abs") :: (read lexbuf)}
	| add 								{(Binary_Op "+") :: (read lexbuf)}
	| sub 								{(Binary_Op "-") :: (read lexbuf)}
	| mul 								{(Binary_Op "*") :: (read lexbuf)}
	| div_ 								{(Binary_Op "div") :: (read lexbuf)}
	| mod_ 								{(Binary_Op "mod") :: (read lexbuf)}
	| expo 								{(Binary_Op "^") :: (read lexbuf)}
	| lparen 							{(Parentheses "(") :: (read lexbuf)}
	| rparen 							{(Parentheses ")") :: (read lexbuf)}
	| true_ 							{(Bool_Const ("T")) :: (read lexbuf)}
	| false_ 							{(Bool_Const ("F")) :: (read lexbuf)}
	| not_ 								{(Unary_Bool ("not")) :: (read lexbuf)}
	| or_ 								{(Binary_Bool ("or")) :: (read lexbuf)}
	| and_								{(Binary_Bool ("and")) :: (read lexbuf)}
	| eq 									{(Comparison_op ("=")) :: (read lexbuf)}
	| gt 									{(Comparison_op (">")) :: (read lexbuf)}
	| lt 									{(Comparison_op ("<")) :: (read lexbuf)}
	| gte 								{(Comparison_op (">=")) :: (read lexbuf)}
	| lte 								{(Comparison_op ("<=")) :: (read lexbuf)}
	| if_ 								{(Conditional_op ("if")) :: (read lexbuf)}
	| then_ 							{(Conditional_op ("then")) :: (read lexbuf)}
	| else_ 							{(Conditional_op ("else")) :: (read lexbuf)}
	| id as i 						{(Id (i)) :: (read lexbuf)}
	| def_ 								{(Def_constr ("def")) :: (read lexbuf)}
	| delimiter_ 					{(Delimiter (";")) :: (read lexbuf)}
  | whitespace 					{read lexbuf}  
	| other_than_all as c {(Invalid (c)) :: (read lexbuf)}
  | ['/''\\'] as c 			{(Invalidc (c)) :: (read lexbuf)}
  | eof 								{[]}
  

{
	let scanner s = read(Lexing.from_string s) 
}	