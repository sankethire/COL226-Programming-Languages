# parser "X" rho;;
- : A1.exptree * A1.value * A1.answer =
(Var "X", NumVal 5, Num (NonNeg, [5]))
# parser "Y" rho;;
- : A1.exptree * A1.value * A1.answer = (Var "Y", BoolVal true, Bool true)
# parser "Z" rho;;
- : A1.exptree * A1.value * A1.answer =
(Var "Z", TupVal (3, [NumVal 5; BoolVal true; NumVal 1]),
Tup (3, [Num (NonNeg, [5]); Bool true; Num (NonNeg, [1])]))