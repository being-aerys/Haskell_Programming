
import Prelude hiding (not)

--Example of a function in Haskell
--A Haskell function to reverse a list of any type of element
reverse_func :: [a] -> [a]
reverse_func [] = []
reverse_func (x:xs) = reverse_func xs ++ [x] --Note that here (x:xs) to the LHS of "=" is a list, not a tuple.


----------------------------------------------------------------------------------------------------------------------------------
--A Haskell function to double an input integer
dbl :: Int -> Int
dbl x = x + x

----------------------------------------------------------------------------------------------------------------------------------
--A Haskell function to add two input integers
add_ints :: Int -> Int -> Int --Understand what these three Ints mean.
add_ints x y = x + y

--Can be called as add_ints 5 6
--We could achieve the same as above using the following function as well.
----------------------------------------------------------------------------------------------------------------------------------
add_ints_new :: (Int,Int) -> Int
add_ints_new (x,y) = x+y
--Should be called as add_ints_new (5,6)

----------------------------------------------------------------------------------------------------------------------------------

--What is the point in having these two ways to achienve the same addition?
--With the first method, you can pass only one argument and get a function as a return value. You can then later pass only one argument to that returned value
--to obtain the final sum. However, with the second method, you are required to pass both the arguments  together.
--This first method comes handy at certain time such as applying a function to a list of values. You will see that belowafter you learn the map funciton.
----------------------------------------------------------------------------------------------------------------------------------

--map functino is predefined in Haskell. It takes two arguments : a function and a list.
--It applies the function to all the elements in the list and returns a list of results.


--map (add_ints 5) [1,2,3] --Applies the add_ints funciton to all the elements in the list. This is not possible with add_ints_new functino defined above.


--------------------------------------------------------------------------------------------------------------------------------------

--Note that there are certain keywords predefined in Haskell and you have to make sure your do not use those names to define your custom data type/ function name.
--You can get around this by either using a different name or "not importing the pre-defined definition"

--For example, the boolean operation not is predefined.
--You can hide hte predefined definition using the following line. But note that Haskell requires you to use this at the beginning of the file.
--import Prelude hiding (not)

not :: Bool -> Bool
not False = True
not True = False

--------------------------------------------------------------------------------------------------------------------------------------------

--PATTERN GUARDS in Haskell

--If you have more than two cases to campare, "if else if....then.... else if ....then....else.... "" looks ugly. In such cases, you can use pattern guards to make your code more presentatble.

--Example of "not using pattern guards"

sign_of_input :: Int -> Int
sign_of_input x = if x > 0 then 1 else if x == 0 then 0 else -1 --looks ugly

--Example of "using pattern guards"

sign_of_inputt :: Int -> Int
sign_of_inputt x | x > 0 = 1
                 | x == 0 = 0
                 | otherwise = -1 --otherwise is just an alias for "True", could replace "otherwise" with "True" here. This indicates that each of the three conditions
                                  -- should be a boolean expression where all  expressions should be False expressions except for "otherwise". This also means that 
                                  --"otherwise" should always be kept as the last expression since it is "True" and the func returns the first "True" encountered.

----------------------------------------------------------------------------------------------------------------------------------------------------------------------

--RECURSION

--Write a function to calculate the factorial of a positive integer.

fact :: Int -> Int
fact 0 = 1
fact x = x * fact(x-1)  --You could also use the pattern guard syntax instead of this.

--Write a function to check whether an input integer is even or odd using recursion (without using % or / operations).


-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

--List in Haskell, Note that you cannot use this in a Haskell file, it is an expresion and should be passed directly to the interpreter.

--[] --List of any type

-- 2:[] --Colon prepends an element to a list. However, colon cannot append an element. i.e., []:2 would be an incorrect syntax

-- 2:3:[]

-- [2,3]

-- [2*3, 4, 5]

-- [1 .. 9]  --generates a list [1,2,3,4,5,6,7,8,9]

-- [True, not True, False, not False]

--[2, 3, [2,3]] --Incorrect syntax, a list should contain the elements of the same type

--You cannot modify a list in Haskell. i.e., you cannot change an element of a list i.e., you cannot delete the element as well.

--head is a predefined funciton that takes in a list and returns the first element
--We can rewrite how head works as below.

headd :: [a] -> a
headd (x:xs) = x --Does not handle the edge case of an empty list as the input argument.

--Handle such edge cases as below:

summ :: [Int] -> Int
summ [] = 0
summ (x:xs) = x + (summ xs)


-----------------------------------------------------------------------------------------------------------------------------------------------------------

--Write a recursive function in Haskell that takes a list of integers as an input, removes all the elements that do not equal to 2, and returns the remaining list.

keep_only_twos :: [Int] -> [Int]
keep_only_twos [] = []
keep_only_twos (x:xs) | x == 2   = 2 : keep_only_twos xs
                      | otherwise  = keep_only_twos xs


------------------------------------------------------------------------------------------------------------------------------------------------------------

--Write a program in Haskell to count the number of 2's in an input list of integers.
--Can reuse the above function rather than use a completely new logic to perform this task.
counts_twos :: [Int] -> Int
counts_twos xs = length (keep_only_twos xs) --xs is an independent list argument.

-------------------------------------------------------------------------------------------------------------------------------------------------------------

--You can see that we used two functins length and keep_only_twos together in the form f (g (x)) --> Function composition allows us to write this as f . g

count_twos :: [Int] -> Int
count_twos = length . keep_only_twos

--------------------------------------------------------------------------------------------------------------------------------------------------------------

--Write a program to check whether an element provided is a member of a list or not.

member_check :: Int -> [Int] -> Bool
member_check x []  = False
member_check x (y:ys) | x == y = True
                      | otherwise = member_check x ys

---------------------------------------------------------------------------------------------------------------------------------------------------------------

--You could rewrite the non-empty case above as below as well.
--member_check x (y:ys) = x == y || member x ys

---------------------------------------------------------------------------------------------------------------------------------------------------------------

--Insert an element at its correct position in an ascending-sorted list.

insert_sorted :: Int -> [Int] -> [Int]
insert_sorted x [] = [x]
insert_sorted x (y:ys) | x <= y   =   x: (y:ys)
                       | otherwise   =   y : insert_sorted x ys

---------------------------------------------------------------------------------------------------------------------------------------------------------------

--You can define a type --> give an alias --> as
type Bag a = [(a, Int)]


--How do you insert an element into a bag.

--ins_bag :: Eq a => a -> Bag a -> Bag a


-----------------------------------------------------------------------------------------------------------------------------------------------------------------

--DATA TYPES in Haskell

data Grade = A | B | C | D | F --Even though A, B, ...., F are data constructors and not values, you can still use the constructor names (i.e., data types) for tasks such as comparison

--Such as below.


is_pass :: Grade -> Bool
is_pass A = True
is_pass B = True
is_pass C = True
is_pass D = True
is_pass F = False

------------------------------------------------------------------------------------------------------------------------------------------------------------------

data Shape = Rectangle Int Int | Square Int | Circle Int | Point Int Int deriving (Eq, Show)

--Rectangle needs length and breadth to construct, Square needs the length of a side, Circle needs a radius, a point needs x and y coordination

-- rect = Rectangle 5 6
-- square = Square 5
-- circle = Circle 2.5



---------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- picture :: [Shape] --Creating a new type alias.
-- picture = [rect, square] --A picture is a list of shapes.


---------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Implementing revmap function i.e., mapping a function to a list after reversing the list


--using function composition
-- revmap :: (a->b) -> [a] -> [b]
-- revmap f [] = []
-- revmap f xs = map f (reverse xs)

--using pattern matching
revmap :: (a->b) -> [a] ->[b]
revmap f [] = []
revmap f (x:xs) = (revmap f xs) ++ f x

--Test the revmap implemented above using revmap (/2) [1,2,3].