# Assignment 0. A Bigint package
#### This is a preliminary assignment which will be used in following assignments.  In this assignment, you will write in OCaml a BIGNUM package  where you will implement arithmetic for arbitrarily large numbers, using lists of digits to implement an integer.



#### The data type can be represented in OCaml as

#### type bigint = sign * int list

#### and sign = Neg | NonNeg;;

#### with the representational invariant that the elements of the int list are between 0 and 9, and are presented most significant digit first, and that there are no unnecessary leading zeros.

#### You will need to implement the following operations in the package:

#### Arithmetic operations:
```
Addition.  add: bigint -> bigint -> bigint
Multiplication.  mult: bigint -> bigint -> bigint
Subtraction.  sub: : bigint -> bigint -> bigint
Quotient:   div: : bigint -> bigint -> bigint
Remainder.  rem: : bigint -> bigint -> bigint 
Unary negation.  minus: bigint -> bigint
Absolute value.  abs: bigint -> bigint
```

#### Comparison operations: 
```
Equal.   eq: bigint -> bigint -> bool
Greater_than.  gt:  bigint -> bigint -> bool
Less_than.  lt:  bigint -> bigint -> bool
Great_or_equal.  geq:  bigint -> bigint -> bool
Less_or_equal.  leq:  bigint -> bigint -> bool
```

#### Functions to present the result in the form of a string. 
```
print_num:  bigint -> string
```
#### Conversion functions from OCaml int to bigint.
```
mk_big:  int -> bigint
```
#### Define suitable exceptions when an operation is not defined.

