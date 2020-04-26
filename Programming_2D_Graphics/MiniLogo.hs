
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
type Num = Int
type Name = String

data Mode = Up | Down
data Pos = Num | Name
data Vals = VALS Num, Vals | Num
data Pars = PARS Name, Pars | Num
data Cmd = Penn Mode | MoveToo Pos Pos | Deff Name (Pars) Cmd | Calll Name (Vals) |  Mult_Cmds Cmd Cmd


---------------------------------------------------------------------------------------------------------------------------------------------------------

--Write a Mini Logo macro vector that draws a line from a given position (x1,y1) to a given position (x2,y2) 
def vector (x1,y1,x2,y2) {
                            MoveToo(x1,y1); 
                            Penn Down; 
                            MoveToo(x2,y2); 
                            Penn Up
                            }  --from "def name ( pars ) cmd" in the production rule of cmd

----------------------------------------------------------------------------------------------------------------------------------------------------------

--Now, represent the macro in abstract syntax, that is, as a Haskell data type value.

vector = Def "vector" (x1, y1, x2, y2) ( MoveToo x1 y1, Penn down, MoveToo x2 y2, Penn up) 



