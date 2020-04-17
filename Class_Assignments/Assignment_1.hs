
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

----------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------Part 2: Graphs-------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------


-----------------IGNORE THE FOLLOWING BLOCK AND CONTINUE-------------------------------------------------------------------------------
import Data.List (nub,sort)
norm :: Ord a => [a] -> [a]
norm = sort . nub
---------------------------------------------------------------------------------------------------------------------------------------



-- A simple way to represent a directed graph is through a list of edges. An edge is given by a pair of nodes. For simplicity, nodes are represented by integers.
type Node = Int
type Edge = (Node,Node)
type Graph = [Edge]
type Path = [Node]

--Say we have the following graphs.
g :: Graph
g = [(1,2),(1,3),(2,3),(2,4),(3,4)]
h :: Graph
h = [(1,2),(1,3),(2,1),(3,2),(4,4)]


--Define the function nodes :: Graph -> [Node] that computes the list of nodes contained in a given graph. For example, nodes g = [1,2,3,4].
nodes :: Graph -> [Node]
nodes [] = []
nodes (x:xs) = norm ([fst x, snd x] ++ nodes xs) --norm was given in the question.


--Define the function suc :: Node -> Graph -> [Node] that computes the list of successors for a node in a given graph. For example, suc 2 g = [3,4], suc 4 g = [], 
--and suc 4 h = [4].
suc x [] = []
suc x ((y1,y2):ys) | x == y1  = [y2] ++ suc x ys
                   | otherwise  = suc x ys

--Define the function detach :: Node -> Graph -> Graph that removes a node together with all of its incident edges from a graph. 
--For example, detach 3 g = [(1,2),(2,4)] and detach 2 h = [(1,3),(4,4)].
detach x [] = []
detach x ((y1,y2):ys) | x == y1 = detach x ys
                      | x == y2   = detach x ys
                      | otherwise = (y1,y2) : detach x ys

--Define the function cyc :: Int -> Graph that creates a cycle of any given number. For example, cyc 4 = [(1,2),(2,3),(3,4),(4,1)].
cyc :: Int -> Graph
helper 1 = []
helper x = (helper (x-1) ++ [(x-1, x)])
cyc x = helper x ++ [(x,1)]
------------------------------------------------------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------Part 3: Programming with Data Types----------------------------------------
----------------------------------------------------------------------------------------------------------------------------


-- A figure is a collection of shapes. The type BBox represents bounding boxes of objects by the points of the lower-left and upper-right hand corners of the
-- smallest enclosing rectangle.

type Number = Int
type Point = (Number,Number)
type Length = Number
data Shape = Pt Point| Circle Point Length| Rect Point Length Length deriving Show --Defines the function show, which converts a value into a string, and other related functions.
type Figure = [Shape]
type BBox = (Point,Point)
------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Define the function width that computes the width of a shape.
width :: Shape -> Length
width (Pt x) = 0
width (Circle pt len) = len * 2
width (Rect pt len widh) = len

--To test, run the following:
--f = [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]
-- map width f
------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Define the function bbox that computes the bounding box of a shape.
bbox :: Shape -> BBox
bbox (Pt p)           = (p, p)
bbox (Circle p l)     = ((fst p - l, snd p - l), ((fst p + l), (snd p + l)))
bbox (Rect p l1 l2)   = (p, ((fst p + l1), (snd p + l2)))

--To test, run the following:
--f = [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]
--map bbox f
------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Define the function minX that computes the minimum x coordinate of a shape
minX :: Shape -> Number
minX (Pt p) = fst p
minX (Rect p l1 l2) = fst p
minX (Circle p l) = fst p - l

--To test, run the following:
--f = [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]
--map minX f 

------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Define a function move that moves the position of a shape by a vector given by a point as its second argument.

addPt :: Point -> Point -> Point
addPt (x1,y1) (x2,y2) = (x1+x2, y1+y2)

move :: Shape -> Point -> Shape
move (Pt p) offs = (Pt (addPt p offs))
move (Rect p l1 l2) offs = (Rect (addPt p offs) l1 l2)
move (Circle p l) offs  = (Circle (addPt p offs) l)

--To test, run the following:
--f = [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]
--move (f!!2) (1,1) 10                          NOTE: !! indexes lists. It takes a list and an index, and returns the item at that index.
------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Define a function alignLeft that transforms one figure into another one in which all shapes have the same minX coordinate but are otherwise unchanged.

moveToX :: Number -> Shape -> Shape
moveToX x_coord (Pt (x,y)) = (Pt (x_coord, y))
moveToX x_coord (Circle (x,y) len) = (Circle (x_coord + len,y) len)
moveToX x_coord (Rect (x,y) len bred) = (Rect (x_coord,y) len bred)

alignLeft :: Figure -> Figure
alignLeft x = map (moveToX (minimum(map minX x))) x

--To test, run the following:
--alignLeft [Pt (0,1),Pt (2,1), Circle (2, 3) 1]
-------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Define a function inside that checks whether one shape is inside of another one, that is, whether the area covered by the first shape is also covered by the second shape.
checking_line :: [Int] -> [Int] -> Bool
checking_line [] [] = True
checking_line (x:xs) (y:ys) | x <= y && checking_line xs ys = True
                            | otherwise = False

checking_inside :: BBox -> BBox -> Bool
checking_inside ((a,b),(c,d)) ((e,f),(g,h)) = (checking_line [e, f, c, d] [a, b, g, h])

inside :: Shape -> Shape -> Bool
inside a b = (checking_inside (bbox a) (bbox b))

--To test, run the following:
--f = [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]
--inside (Circle (2,2) 2) (Rect (3,3) 3 3 )
--inside (Circle (2,2) 2) (Circle (2,2) 3)
-------------------------------------------------------------------------------------------------------------------------------------------------------------------

