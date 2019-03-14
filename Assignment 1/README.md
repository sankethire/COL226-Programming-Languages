
#### Assignment 1: A simple definitional interpreter and stack machine
In this assignment, you will model the "abstract syntax" of a simple calculator language for integer expressions, and give it "semantic meaning" in terms of OCaml's built-in types, and in the second part, implement the calculator as a simple stack-based machine, for which we have opcodes into which we compile the abstract syntax of an expression.

The abstract syntax is characterised by the type
```
type  exptree =  N of int 

                        |  Plus of exptree *  exptree 

                        | Minus of exptree *  exptree 

                        |  Mult of exptree *  exptree 

                        | Div of exptree *  exptree 

                        | Rem of exptree *  exptree 

                        | Neg of  exptree 

                        | Abs of  exptree 

                        ;;
```
The definitional interpreter is to be defined as a function 
eval : exptree -> int
The type of opcodes of the stack machine is defined as:
```
type opcode = CONST of bigint | PLUS | TIMES | MINUS | DIV | REM | ABS | UNARYMINUS ;;
```
For simplicity the stack will be implemented as a list of bigint values, and the program is a list of opcodes.  If you have not implemented the bigint package, you may use int for reduced credits.

The stack machine is to be defined as a tail-recursive function
```
stackmc: (bigint list) -> (opcode list) -> bigint
```
The compile function is simply a postorder traversal of an abstract syntax tree of an expression

The compiler is to be defined as a recursive function
```
compile: exptree -> opcode list
```
As a paper exercise, you have to state and prove the theorem that your compiler+stack machine calculates a bigint whose value is the same as what eval computes.
