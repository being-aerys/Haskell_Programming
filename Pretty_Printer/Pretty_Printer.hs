


--Pretty Printer for Digital Circuit Design Language

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

-- Define a Haskell function that implements a pretty printer for the abstract syntax.


pretty_printer_int_to_str :: Int -> String
pretty_printer_int_to_str 0 = "0"
pretty_printer_int_to_str 1 = "1"
pretty_printer_int_to_str 2 = "2"
pretty_printer_int_to_str 3 = "3"
pretty_printer_int_to_str 4 = "4"
pretty_printer_int_to_str 5 = "5"
pretty_printer_int_to_str 6 = "6"
pretty_printer_int_to_str 7 = "7"
pretty_printer_int_to_str 8 = "8"
pretty_printer_int_to_str 9 = "9"


pretty_printer_list_of_links_aux :: Link -> String
pretty_printer_list_of_links_aux (x1, y1, x2, y2) = "from "++ pretty_printer_int_to_str x1 ++"." ++ pretty_printer_int_to_str y1++" to "++ pretty_printer_int_to_str x2 ++"."++ pretty_printer_int_to_str y2 ++";\n"


pretty_printer_list_of_links :: List_of_Links -> String
pretty_printer_list_of_links [] = ""
pretty_printer_list_of_links (x:xs) = pretty_printer_list_of_links_aux x ++ pretty_printer_list_of_links xs


pretty_printer_gate_func :: GateFunc -> String
pretty_printer_gate_func AND = ":and;\n"
pretty_printer_gate_func OR  = ":or;\n"
pretty_printer_gate_func XOR = ":xor;\n"
pretty_printer_gate_func NOT = ":not;\n"


pretty_printer_gates_aux :: Gate -> String
pretty_printer_gates_aux (x, y) = pretty_printer_int_to_str x ++ pretty_printer_gate_func y


pretty_printer_gates :: Gates -> String
pretty_printer_gates [] = ""
pretty_printer_gates (x:xs) = pretty_printer_gates_aux x  ++ pretty_printer_gates xs


pretty_printer_circuit :: Circuit -> String
pretty_printer_circuit (Ckt x y) = pretty_printer_gates x ++ pretty_printer_list_of_links y

------------------------------------------------------------------------------------------------------------------------------------------------------------------
