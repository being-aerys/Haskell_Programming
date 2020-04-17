
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
moveToX x_coord (Pt (x,y))             = (Pt (x_coord, y))
moveToX x_coord (Circle (x,y) len)       = (Circle (x_coord + len,y) len)
moveToX x_coord (Rect (x,y) len bred)     = (Rect (x_coord,y) len bred)

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

