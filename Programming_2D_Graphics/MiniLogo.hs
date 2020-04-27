
--Following is a grammar to draw shapes using a plotter which has a pen that can be lifted up and pushed down as required to draw lines on the canvas.

--From the question, Following operations and values are terminals: pen, moveto, def, call, up, down.
--Following are non-terminals: mode, pos, name, pars, cmd, name, vals

-- cmd ::= pen mode                 --pen is an operation with an argument named mode passed to itm mode can have two values: up or down since a pen can be either lifted or pushed down
-- | moveto (pos,pos)               --moveto is an operation with a tuple of pos data types passed as arguments
-- | def name ( pars ) cmd          --def is an operation that takes two arguments "name" and "pars". Here, pars is short for parameters. Notice: looks like a function defn.
-- | call name ( vals )             --call is a name for an operation that takes two arguments namely "name" and "(vals)"
-- | cmd; cmd                       --cmd;cmd means there can be two cmd statements, separated using a ; as you would in C or Java
-- mode ::= up | down
-- pos ::= num | name
-- pars ::= name, pars | name
-- vals ::= num, vals | num


--First, lets give alias names for the non-primitive but terminal symbols in the given abstract grammar using hte "type" keyword.
type Numm = Int
type Name = String
type List_Cmd = [Cmd] --Need as an auxilliary to define the function "steps"

data Mode = Up | Down
data Pos = Pos_Num Numm | Pos_Name Name
data Vals = Vals_Num_Vals Numm Vals | Vals_Num Numm
data Pars = Pars_Name_Pars Name Pars | Pars_Name Name
data Cmd = Penn Mode | MoveToo (Pos, Pos) | Def Name (Pars) Cmd | Calll Name (Vals) |  Mult_Cmds Cmd Cmd


---------------------------------------------------------------------------------------------------------------------------------------------------------

--Write a Mini Logo macro "vector" that draws a line from a given position (x1,y1) to a given position (x2,y2) --> means write a CONCRETE SYNTAX
-- def vector (x1,y1,x2,y2) { MoveToo (x1,y1); --Assuming the pen is already up by default.
--                             Penn Down; 
--                             MoveToo (x2,y2); 
--                             Penn Up
--                             }  --from "def name ( pars ) cmd" in the production rule of cmd



----------------------------------------------------------------------------------------------------------------------------------------------------------

--Now, represent the macro in abstract syntax, that is, as a Haskell data type value.

vector = Def "vector" (Pars_Name_Pars "x1" (Pars_Name_Pars "y1" (Pars_Name_Pars "x2" (Pars_Name "y2"))))  (Mult_Cmds (Mult_Cmds (MoveToo (Pos_Name "x1", Pos_Name "y1")) (Penn Down)) (Mult_Cmds (MoveToo (Pos_Name "x2", Pos_Name "y2")) (Penn Up)))

------------------------------------------------------------------------------------------------------------------------------------------------------------

--Define a Haskell function steps :: Int -> Cmd that constructs a Mini Logo program which draws a stair of n steps. Your solution should not use the macro vector.

steps :: Int -> List_Cmd
steps 0 = []
steps num_of_steps = [ Penn Up, MoveToo(Pos_Num num_of_steps, Pos_Num num_of_steps), Penn Down] ++ stairs(num_of_steps)

stairs :: Int -> List_Cmd
stairs 0 = []
stairs num_of_steps = [ MoveToo(Pos_Num (num_of_steps-1), Pos_Num num_of_steps),MoveToo(Pos_Num (num_of_steps-1), Pos_Num (num_of_steps-1))] ++ stairs(num_of_steps-1)

---------------------------------------------------------------------------------------------------------------------------------------------------------------------

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
