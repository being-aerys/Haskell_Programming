
-----------------------------------------------------------------------------------------------------------------------
-------------------------------------------PART 1: Bag/ MultiSet in Haskell--------------------------------------------
-----------------------------------------------------------------------------------------------------------------------

--Multisets, or bags, can be represented as list of pairs (x, n) where n indicates the number of occurrences of x in the multiset.
--A type Bag/ Multiset is defined as below.
type Bag a = [(a,Int)]


--Define a function that inserts an element into a multiset
ins :: Eq a => a -> Bag a -> Bag a --Here "Eq a ==>" means that the element type a should be one of the data types that allow the comparison of this element to another entity using the == operator
ins x [] = [(x,1)]
ins x ((x_bar, n) : xs)
    |x == x_bar = (x_bar,n+1):xs 
    |otherwise =  ((x_bar,n) : ins x xs)
---------------------------------------------------------------------------------------------------------------------------------------------------------------

--Define a function del that removes an element from a multiset.
del :: Eq a => a -> Bag a -> Bag a
del x [] = []
del x ((x_bar,n):xs)
    | x == x_bar  && n == 1 = xs
    | x == x_bar && n > 1 = ((x_bar,n-1) : ins x xs)
    | otherwise = ( (x_bar,n) : del x xs )
----------------------------------------------------------------------------------------------------------------------------------------------------------------

--Define a function bag that takes a list of values and produces a multiset representation.
emptyBag :: Bag a
emptyBag = []

bag :: Eq a => [a] -> Bag a
bag [] = []
bag (x:xs) = ins x (bag xs)
-----------------------------------------------------------------------------------------------------------------------------------------------------------------  

--Define a function subbag that determines whether or not its first argument bag is contained in the second.

checker x []     = False
checker x (y:ys) | x == y = True
               | otherwise = checker x ys

subbag :: Eq a => Bag a -> Bag a -> Bool
subbag xs [] = False
subbag [] ys = True
subbag (x:xs) ys = checker x ys && subbag xs ys
-----------------------------------------------------------------------------------------------------------------------------------------------------------------

--Define a function isbag that computes the intersection of two multisets.
isbag :: Eq a   => Bag a -> Bag a -> Bag a
isbag [] ys     = []
isbag (x:xs) ys | checker x ys = x : isbag xs ys
                | otherwise  = isbag xs ys
-----------------------------------------------------------------------------------------------------------------------------------------------------------------

--Define a function size that computes the number of elements contained in a bag.
size :: Bag a -> Int
size [] = 0
size (x:xs) = snd x + size xs
------------------------------------------------------------------------------------------------------------------------------------------------------------------
