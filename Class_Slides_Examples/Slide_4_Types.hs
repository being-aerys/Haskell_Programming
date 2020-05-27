

--TYPE-CHECKING (From files Expr2.hs)

--We earlier saw in the arithmetic expression language that we could handle errors by returning Nothing as the return type in the semantics definition.













--Work on this task here and then move to the following part.
--------------------------------------------------------------------------------------------------------------------------------------------------

--Writing unsafe semantic definitions

data Expr = N Int | Plus Expr Expr | Equal Expr Expr | Not Expr deriving Show


eval :: Expr -> Int
eval (N i)        = i
eval (Plus e e')  = eval e+eval e'
eval (Equal e e') | eval e == eval e' = 1                  
                  | otherwise         = 0
eval (Not e)      | eval e==0 = 1                  --Using 0 and 1 for False and True
                  | otherwise = 0

--for this however, we need to define true and false ourselves first. Not understood well.
true = N 2
false = N 1 `equal` N2

--This semantic definition allows for the addition of an interger and a boolean value which is unsafe.

--------------------------------------------------------------------------------------------------------------------------------------------------

--We have already seen dynamic type checking for Arithmetic Expression language in the Semantics slide.
--Now lets create a dedicated type checker instead. --> Static Type Checker that checks if there is a type mismatch before trying to evaluate the expression.
--A typechecker is a function that takes a program as an input and returns the type of the output.
--For our "Arithmetic Expression" language/ data type that we have been using for a while, a type checker would take an Arithmetic Expression as an input
--and return the type of the result.
--However, note that we need to pre-define a data type that collectively represents all the possible return types of an expression including the "error" type.

data Type = Int | Bool | TypeError deriving (Eq, Show) --VVI: These Int and Bool are not integer and boolean data types in Haskell, they are just constructor names. 


tc :: Expr -> Type
tc (N i)                                    = Int
tc (Plus e e')  | tc e==Int  && tc e'==Int  = Int
tc (Equal e e') | tc e==Int  && tc e'==Int  = Bool                
                | tc e==Bool && tc e'==Bool = Bool
                --We donot have to address the cases where e and e' are of mismatch types Bool and Int here for "Equal e e' " because if the pattern doesnt match, it proceeds and reaches the last case of TypeError.
tc (Not e)      | tc e==Bool                = Bool
tc _                                        = TypeError


--Now allow to check whether an expression is type correct or not using the typechecker function defined above.
typeCorrect :: Expr -> Bool
typeCorrect e = tc e /= TypeError

--Now use the following function to evaluate only those expressions that are type-correct.
evalStatTC :: Expr -> Maybe Val
evalStatTC e | typeCorrect e = Just (eval e)             
             | otherwise     = Nothing
             


-- e1 = N 2 `Equal` N 2
-- e2 = N 1 `Equal` e1

-- compare "evalStatTC e2" and "eval e2"