

-------------------------------------------------------Defining a function in Haskell----------------------------------------------------------------------

--Triple a number 
triple_a_number::
 Int -> Int --means a function  named triple_a_number that takes an integer as an input and returns an integer as an output
triple_a_number x = 2 *x

-----------------------------------------------------------------
--Add two numbers
add_two_nums:: Int -> Int -> Int
add_two_nums x y = x+y

-----------------------------------------------------------------

--The function above is equivalent to the function below.
add_two_nums_new:: Int->Int-> Int
add_two_nums_new x y = x+y

------------------------------------------------------------------
--The difference between the above two definitions is that the first version always requires two arguments while the second version can work only one argument as well.
--This has a significant advantage which you will see below.
------------------------------------------------------------------

--map (add_two_nums_new 3) [4,5,6] --This expression adds 3 to the elements of the list individually. However, you cannot put expression in a file.
--This should be run on the console.

------------------------------------------------------------------
--Larger of the two numbers
larger_func::Int->Int -> Int
larger_func x y = if x >= y then x else y

-------------------------------------------------------------------

--Reversing a Boolearn input
not_gate::Bool->Bool
not_gate x = if x == True then False else True



--The not gate can be simplified as below as well

not_gate_new::Bool->Bool
not_gate_new x = if x then False else True

--It can further be simplified as

not_gate_bar::Bool->Bool
not_gate_bar True = False
not gate_bar False = True

----------------------------------------------------------------------
--Else-If statement

--Return the sign of an input integer
sign_func::Int->Int
sign_func x = if x >0 then 1 else if x<0 then -1 else 0


--------------------------------foldl function meaning----------------------------------------
--https://www.burgaud.com/foldl-foldr-python
--Let ⊗ be an operation that is to be passed as the first argument to the foldl function. Then, the working of foldl function looks as below:

--foldl (⊗) z [1,2,3,4,5]=((((z ⊗ 1) ⊗ 2) ⊗ 3) ⊗ 4) ⊗ 5

--To cement the concept, here is an example in Haskell with the subtraction operator as the ⊗ operation:

--      foldl (-) 0 [1,2,3] gives
--      (((0 - 1) - 2) - 3)
--      6


--Flip simply takes a function and returns a function that is like our original function, only the first two arguments are flipped.

--------------------------------------------------------------------------------------------------------------------------------------------------

--where keyword in Haskell

--Say you have a function that u use to calculate the BMI score, i.e., BMI = weight / (height * height) (taken from learnyouahaskell.com)

categorize_students :: Float -> Float -> String
categorize_students weight height | weight/ (height * height ) <= 18.5      =  "Underweight"
                                  |  weight/ (height * height ) <= 25.5     =  "Normal BMI"
                                  |  weight/ (height * height ) <= 30       =  "Some extra weight that normal"
                                  |  otherwise                              =  "Overweight"


--As u can observe above, you calculated a value 3 times to compare with some number. For a large program with such calculations, it would be better
--to calculate such value once and then use the calculated value to compare. Thus enters the haskell eyword "where".

categorize_with_where:: Float -> Float -> String
categorize_with_where weight height | bmi <= 18.5    = "underweight"
                                    | bmi  <= 25.5   = "normal"
                                    | bmi  <= 30     = "some extra weight than normal"
                                    | otherwise       = "overweight"
                                    where bmi = weight / height^ 2

-----------------------------------------------------------------------------------------------------------------------------------------------------

--let keyword
--let keyword in Haskell allows us to define subexpressions within the expression that we want to calculate as the definition of a function.
--This will be obvious from the example below. (taken from learnyouahaskell.com)

cylinder :: Float -> Float -> Float
cylinder radius height = let sideArea = 2 * pi * radius * height 
                             topArea = pi * radius ^2  
                             in  sideArea + (2 * topArea)  --This last part is what the function returns

-----------------------------------------------------------------------------------------------------------------------------------------------------
