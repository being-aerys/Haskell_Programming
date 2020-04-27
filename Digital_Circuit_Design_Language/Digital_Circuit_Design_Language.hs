
--Digital Circuit Design Language

{- 

The “Digital Circuits Design Language” whose syntax is shown below can be used to describe circuits built from
these gates.
circuit ::= gates links
gates ::= num:gateFn ; gates | ϵ
gateFn ::= and | or | xor | not
links ::= from num.num to num.num; links | ϵ

-}

------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Define the abstract syntax for the above language as a Haskell data type.

data GateFunc = AND | OR | XOR | NOT
type Gate = (Int, GateFunc)
type Gates = [Gate]
type Link = (Int, Int, Int, Int)
type List_of_Links =  [Link]
data Circuit = Ckt Gates List_of_Links

--------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-
A half-adder circuit can be represented by the following DiCiDL program.

1:xor;
2:and;
from 1.1 to 2.1;
from 1.2 to 2.2;

 -}

--Represent the half adder circuit in abstract syntax, that is, as a Haskell data type value

half_adder =  Ckt [(1, XOR), (2, AND)]  [(1,1,2,1), (1,2,2,2)]

---------------------------------------------------------------------------------------------------------------------------------------------------------------------


