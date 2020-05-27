
--------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------Denotational Semantics for Boolean Expressions-------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------

--Create a Boolean expression data type.

data BExp = T | F | Not BExp | Or BExp BExp  deriving Show -- Here, T, F, Not and Or are just data constructor names and themselves do not have any value.

--Lets define a Boolean Expression as
not_not_True :: BExp --This means that not_not_True is a boolean expresion data typed defined as above.
not_not_True = Not (Not T)  --Now, note that here, Not and T do are not predefined binary operation "not" and boolean value "True". They have been burrowed from the
                            -- BExp data type above.


true_or_not_not_true :: BExp
true_or_not_not_true = Or T not_not_True
-------------------------------------------------------------------------------------------------------------------------------------------------------

--So we have two boolean expressions not_not_true and true_or_not_not_true.
--Now if you run non_not_ture in ghci interpreter, it will return (Not (Not T)). <-- This is not a semantically-sound values of the expression
--It is the task of the semantic definition to provide the correct evaluation for the boolean expression type BExp.
--So we need to define a semantic definition for the BExp type.

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
--Here, BExp is a datatype --> Gives a grammar/ language
--Semantic Domain of this language --> What this language maps to.
--As we know that BExp maps to a boolean (is supposed to even though we have not explicitly made it available in the syntax above)
--Thus. the semantic domain of this language is Boolean. We have to define the semantic domain as a function ourself. Lets do that now.

semantic_meaning:: BExp -> Bool --Transforming syntactic domain "BExp" to semantic domain "Bool"
semantic_meaning T = True
semantic_meaning F = False
semantic_meaning (Not x) = not (semantic_meaning x)
semantic_meaning (Or x y) = semantic_meaning(x) || semantic_meaning(y) --Note that for each constructor in the styntactic domain, you need at least one definition 
                                                                       -- in the semantic domain as in this example.

--call using semantic_meaning (Or T not_not_True) in ghci interpreter.


--------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------Denotational Semantics for Arithmetic Expressions-------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------


--Define a data type for arithmetic expressions first.

data Arith_Exp = Num Int | Plus Arith_Exp Arith_Exp | Neg Arith_Exp | Mult Arith_Exp Arith_Exp deriving (Show)

--Lets define the semantics of the language/ data type above. We expect an arithmetic expression to return an integer. Hence the semantics function for this language
--will convert the input Arith_Exp data type into an integer data type.

sem :: Arith_Exp -> Int
sem (Num x) = x         --sem Num x returns an error that the syntax has a different number of arguments.
sem (Plus x y) = sem(x) + sem(y)
sem (Neg exp) = -(sem(exp))
sem (Mult x y) = (sem x) * (sem y) 

--Lets create an arithmetic expression and then evaluate its true semantic value using the semantic function we just defined.

expp = Plus (Neg (Num 5)) (Num 7)

--Now call "sem expp" from the ghci interpreter to get 2 as the answer.



--------------------------------------------------------------------------------------------------------------------------------------------------

--HANDLING ERRORS USING SEMANTIC DOMAIN

--Lets define a data type arithmetic expression similar as above but now includes divisions as well.
--Division by a zero shud return an error. That is what we will try to do below by returning "Nothing" as the result whenever we encounter a division
--by zero.

data Arith_Expr = Numm Int | D Arith_Expr Arith_Expr deriving Show --Avoiding the addition, subtraction, and multiplication for the ease of explanation.

semm :: Arith_Expr -> Maybe Int     --Maybe allows us to handle errors
semm (Numm num) = Just num
semm (D x y) =  case (semm x, semm y)  of   
                            (Just sem_exp1, Just sem_exp2)  -> if  sem_exp2/= 0 then Just (sem_exp1 `div` sem_exp2)  else Nothing --This part shud be on a new line.
                            _                               -> Nothing

                            --VVI: here sem_exp1 and sem_exp2 mean the returned evaluated values of div_semantics x and div_semantics y
                            --VVI: pattern matching of 

--------------------------------------------------------------------------------------------------------------------------------------------------

--UNION OF SEMANTIC DOMAINS

--Lets write a data type for arithmetic expression as above but with a privision for equality checks as well.

data Arith_Exp = Num Int | Plus Arith_Exp Arith_Exp | Neg Arith_Exp | Mult Arith_Exp Arith_Exp | Equal Arith_Exp Arith_Exp deriving (Show)

--Now u can see that while all other constructors need to return an integer, the Equal constructor needs to return a Boolean value after
--operation.

--So we can define a data type that accomodates both Bool and Int values.

data Val = Either Int Bool deriving Show
--We could have defined it as --> data val = I Int | B Bool   OR as --> type val = Either Int Bool

--Why we did this is because now in the semantic function definition, we can put val as the return type  in the type signature on top instead of
--Int (Or Maybe Int) as we did in the semantic function definition above.


semmm :: Expr -> Val
semmm (N i)        = I i
semmm (Plus e e')  = case (semmm e,semmm e') of
                     (I i,I j) -> I (i+j)               --VVI: This pattern matching for I data type is very important to distinguish between the returned evaluated value fo the e and e'.
semmm (Equal e e') = case (sem e,semmm e') of           --Here, as you can see, we have not handled error and thus have not used Nothing case.
                     (I i,I j) -> B (i==j)              --If we had thought of handling errors, the return type in the type signature would have been Maybe Val
                     (B b,B c) -> B (b==c)
semmm (Not e)      = case semmm e of
                     B b  -> B (not b)