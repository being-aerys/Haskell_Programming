--We extend the simple stack language from Homework 2

--INC increments the topmost element on the stack
--SWAP exchanges the two topmost elements on the stack, and
--POP k pops k elements of the stack.

data Cmd = LD Int | ADD | MULT | DUP | INC | SWAP | POP Int
type Prog = [Cmd]

--The rank of a stack is given by the number of its elements.
--A type system can be defined for this language that assigns ranks to stacks and operations and ensures that a program does not result in a rank mismatch.

--The rank of a single stack operation is given by a pair of numbers (n, m) where n indicates the number of elements the operation takes from
--the top of the stack and m is number of elements the operation puts onto the stack.

--The rank for a stack program is defined to be the rank of the stack that would be obtained if the program were run on an empty stack.
--A rank error occurs in a stack program when an operation with rank (n, m) is executed on a stack with rank k < n. In other words, a rank error indicates a stack underflow

type Rank = Int
type CmdRank = (Int,Int)  --Rank of single stack operation/ command

--Define a function rankC that maps each stack operation to its rank.
rankC :: Cmd -> CmdRank
rankC (LD inte) = (0,1)
rankC ADD       = (2,1)
rankC MULT      = (2,1)
rankC DUP       = (1,2)
rankC INC       = (1,1)
rankC SWAP      = (2,2)
rankC (POP inte) = (inte,0)


-- Define a function rankP that computes the rank of a program. The Maybe data type is used to capture rank errors, that is, 
--a program that contains a rank error should be mapped to Nothing whereas ranks of other programs are wrapped by the Just constructor.
--You might need to define an auxiliary function rank :: Prog -> Rank -> Maybe Rank and define rankP using rank.


rank :: Prog -> Rank -> Maybe Rank
rank [] rankk               = Just rankk
rank (p:rest_of_prog) rankk =  let (x,y) = rankC p in  if x > rankk then Nothing else rank rest_of_prog ((rankk-x)+y)

rankP :: Prog -> Maybe Rank
rankP []       = Just 0
rankP  program = rank program 0 


----------------------------------------------------------------------------------------------------------------------------------------------------

-- Following the example of the function evalStatTC (defined in the file TypeCheck.hs), define a function semStatTC for evaluating stack programs 
--that first calls the function rankP to check whether the stack program is type correct and evaluates the program only in that case. For performing 
--the actual evaluation semStatTC calls the function sem. Note that the function sem called by semStatTC can be simplified. Its type can be simplified
--and its definition. What is the new type of the function sem and why can the function definition be simplified to have this type?
--(You do not have to give the complete new definition of the function.)

--Defining a typechecker to ensure that the resultant would not be a TypeError.

type Stack = [Int]

semCmd :: Cmd -> Stack -> Stack
semCmd (LD inte) xs  = (inte:xs)
semCmd ADD (x:y:xs)  = ((x+y):xs)
semCmd MULT (x:y:xs) = ((x*y):xs)
semCmd DUP (x:xs)    = (x:x:xs)
semCmd INC (x:xs)    = ((x+1):xs)
semCmd SWAP (x:y:xs) = (y:x:xs)
semCmd (POP inte) xs = (drop inte) xs

sem :: Prog -> Stack
sem = foldl (flip semCmd) [] 

semStatTC :: Prog -> Maybe Stack
semStatTC program | rankP program == Nothing  = Nothing 
                  | otherwise                 = Just (sem program)



-- semTemp :: Prog -> Stack -> Stack
-- semTemp [] s = s
-- semTemp (x:xs) s = semTemp xs (semCmd x s)

-- semStatTC :: Prog -> Maybe Stack
-- semStatTC program | rankP program == Nothing  = Nothing 
--                   | otherwise                 = Just (semTemp program)


----foldl (⊗) z [1,2,3,4,5]=((((z ⊗ 1) ⊗ 2) ⊗ 3) ⊗ 4) ⊗ 5
--Flip simply takes a function and returns a function that is like our original function, only the first two arguments are flipped.


