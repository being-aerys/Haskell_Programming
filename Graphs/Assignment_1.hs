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
