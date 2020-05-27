
-- Consider the stack language S defined by the following grammar.
-- S ::= C | C;S
-- C ::= LD Int | ADD | MULT | DUP

type Prog = [Cmd]

data Cmd = LD Int| ADD| MULT| DUP

type Stack = [Int]

--Lets understand what the question means:
-- Question says --> Define the semantics for the stack language as a Haskell function sem that yields the semantics of a program. 
--This means define a function that takes in a stack (which is a list of Ints from the type definition above) and outputs something in the proper semantic domain.
--So what is this proper semantic domain???

--The question says --> Please note that the semantic domain has to be defined as a function domain (since the meaning of a stack program is
-- a transformation of stacks) and as an error domain (since operations can fail). Therefore, sem has to have the following type where you have to find an appropriate
--type defintition for D.   --> sem :: Prog -> D

--So what should be the appropriate type of this D?????
--In haskell, to handle errors,we use the Maybe type. Andretips.com states the following:

-- Often we want to write a function that just doesn’t make sense for all possible inputs.
--For example, the head function head extracts the first element from a list.
--It works great for most lists, but for empty lists, it crashes your program.
--In other languages, what you might do is return some value like null or None.


--Haskell provides a better way, using Maybe. Here’s how it works.

-- maybeHead.hs
-- maybeHead :: [a] -> Maybe a
-- maybeHead (x:xs) = Just x
-- maybeHead _ = Nothing

--In our case, we want to return a stack state  (i.e., a list of Ints) if the operations succeed and if it does not, we need to handle the exception using Maybe.
--Hence, we provide an alias for type D as below:

-- type D = May





--An auxiliary function that defines the semantics of the individual operations LD, ADD, MULT, DUP
-- semCmd :: Cmd -> D

semCmd :: Cmd -> 0
semCmd (LD x) xs           = Just (x:xs)   --For each syntactic constructor in data Cmd = LD Int| ADD| MULT| DUP, define at least one operation
semCmd ADD (x : y: xs)     = Just((x+y) : xs)
semCmd MULT (x:y:xs)       = Just((x * y) : xs)
semCmd DUP (x:xs)          = Just([x,x] ++ xs)
semCmd _ _                 = Nothing

--Defining the semantic domain for the Stack language as a Haskell function.
sem :: Prog -> Maybe Stack -> Maybe Stack
sem [] (Just s) = Just s
sem [] Nothing = Nothing
sem (p:ps) (Just s) = sem ps (semCmd p s)
sem (p:ps) Nothing = Nothing

--To test the above program, run the following command on the ghci interpreter.
--sem  [LD 3,DUP,ADD,DUP,MULT] (Just []) 


----------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Abstract Syntax for MiniLogo program
data Cmd = Pen Mode| MoveTo Int Int| Seq Cmd Cmd
data Mode = Up | Down


type State = (Mode,Int,Int)                                 --state of the pen that draws

type Line = (Int,Int,Int,Int)                               --semantic domain representing a set of drawn lines 
type Lines = [Line]


semS :: Cmd -> State -> (State,Lines)                       --This function defines for each Cmd how it modifies the current drawing state and what lines it produces
semS (Pen m) (m_, x, y) = ((m, x, y),)



