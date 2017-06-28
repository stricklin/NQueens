> import System.IO
> import Data.List
> import Data.Matrix

_______________________________________________________________________________
listToRowCol is a helper function that turns the list of rows into a list
of rows and columns.

> listToRowCol xs = zip xs [0..]

Main> listToRowCol failingBoard 
[(0,0),(1,1),(2,2),(3,3)]
Main> listToRowCol passingBoard 
[(1,0),(3,1),(0,2),(2,3)]
Main> listToRowCol failingBoard 
[(0,0),(1,1),(2,2),(3,3)]
_______________________________________________________________________________

positionPairs takes a list and returns a list of pairs where the first element
of each pair is the head of that list and the second element is each other 
element in order. 
A list with a single element evaluates to the empty list

> positionPairs :: [b] -> [(b, b)]
> positionPairs (xy:xys) = zip (repeat xy) xys
> positionPairs xy = []

Main> positionPairs ["a", "b", "c", "d", "e"]
[("a","b"),("a","c"),("a","d"),("a","e")]
_______________________________________________________________________________

checkDiagonal checks two positions for diagonal threats.
It does this by getting the differences between the x cordinates and y
cordinates respecitvely. If they have the same absolute value, that indicates
they threaten eachother.
Evauates to true if there is a threat.

 checkDiagonal :: ((Integer, Integer), (Integer, Integer))-> Bool

> checkDiagonal (xy1, xy2) = abs(fst xy1 - fst xy2) == abs(snd xy1 - snd xy2)

Main> checkDiagonal ((1,1),(1,1))
True
Main> checkDiagonal ((1,1),(2,2))
True
Main> checkDiagonal ((1,2),(2,1))
True
Main> checkDiagonal ((0,0),(1,2))
False
Main> checkDiagonal ((0,0),(2,3))
False
_______________________________________________________________________________

checkBoard checks if a board is a valid arrangement of queens.
It does this by first turning the board into a list of row column pairs,
then getting a list of the tails of that list,
then getting a list of "position pairs" of that list (at this point every 
possible pair of positions is in the list (not counting different orderings as
different pairs)),
then each of these pairs is checked for diagonal threats.
The horizontal and vertical threats do not neet to be checked as long as each
number is only in the board once.

> checkBoard board = all not $ concat $ map (map checkDiagonal) $ map positionPairs $ tails $ listToRowCol $ board

> passingBoard = [1, 3, 0, 2]
> failingBoard = [0, 1, 2, 3]
> testBoards = [passingBoard, failingBoard]

*Main> checkBoard passingBoard 
True
*Main> checkBoard failingBoard 
False
_______________________________________________________________________________

makeBoards will make all the possible boards with no queens threatening 
horizontally or vertically

> makeBoards n = permutations [0..n-1]

*Main Data.Matrix> makeBoards 0
[[]]
*Main Data.Matrix> makeBoards 1
[[0]]
*Main Data.Matrix> makeBoards 2
[[0,1],[1,0]]
*Main Data.Matrix> makeBoards 3
[[0,1,2],[1,0,2],[2,1,0],[1,2,0],[2,0,1],[0,2,1]]
_______________________________________________________________________________

toMatrix turns a list of row positions into a matrix where queen positions
are marked with 1s.

> toMatrix board = matrix (length board) (length board) (\(i,j) -> if i == (board!!(j-1))+1 then 1 else 0)

*Main> toMatrix [0,1,2,3,4]
( 1 0 0 0 0 )
( 0 1 0 0 0 )
( 0 0 1 0 0 )
( 0 0 0 1 0 )
( 0 0 0 0 1 )

*Main> toMatrix [4,3,2,1,0]
( 0 0 0 0 1 )
( 0 0 0 1 0 )
( 0 0 1 0 0 )
( 0 1 0 0 0 )
( 1 0 0 0 0 )
_______________________________________________________________________________

nqueens evaluates to a list of nxn matrix boards of all valid nqueens solutions

> nqueens n = map toMatrix $ filter(checkBoard) $ makeBoards n

*Main Data.Matrix> nqueens 0
[]
*Main Data.Matrix> displayBoards $ nqueens 0
*Main Data.Matrix> displayBoards $ nqueens 1
[1]
*Main Data.Matrix> displayBoards $ nqueens 2
*Main Data.Matrix> displayBoards $ nqueens 3
*Main Data.Matrix> displayBoards $ nqueens 4
[0,0,1,0]    [0,1,0,0]
[1,0,0,0]    [0,0,0,1]
[0,0,0,1]    [1,0,0,0]
[0,1,0,0]    [0,0,1,0]
_______________________________________________________________________________

displayBoards displays a list of matrix boards side by side

> displayBoards [] = putStr "Nothing to display\n"
> displayBoards boards = putStr
> 			 .unlines.map(foldr1(\xs ys->xs++ "    " ++ys))
>			 $  map(map (show)) 
>			 $ Data.List.transpose 
>			 $ map(toLists) boards 

*Main Data.Matrix> displayBoards []
Nothing to display

*Main Data.Matrix> displayBoards $nqueens 4
[0,0,1,0]    [0,1,0,0]
[1,0,0,0]    [0,0,0,1]
[0,0,0,1]    [1,0,0,0]
[0,1,0,0]    [0,0,1,0]
_______________________________________________________________________________

top3x3 and left3x3 are both matrices for testing reflections and rotations

> top3x3 =  matrix 3 3 (\(x,y) -> if x==1 then 1 else 0)
> left3x3 =  matrix 3 3 (\(x,y) -> if y==1 then 1 else 0)

*Main Data.Matrix> top3x3 
( 1 1 1 )
( 0 0 0 )
( 0 0 0 )

*Main Data.Matrix> left3x3 
( 1 0 0 )
( 1 0 0 )
( 1 0 0 )
_______________________________________________________________________________

xReflection reflects a matrix on its x axis

> xReflection matrix = fromLists $ reverse $ toLists matrix

*Main Data.Matrix> XReflection top3x3 
( 0 0 0 )
( 0 0 0 )
( 1 1 1 )

*Main Data.Matrix> XReflection left3x3 
( 1 0 0 )
( 1 0 0 )
( 1 0 0 )
_______________________________________________________________________________

yReflection reflects a matrix on its y axis

> yReflection matrix = fromLists $ map reverse $ toLists matrix

*Main Data.Matrix> YReflection top3x3 
( 1 1 1 )
( 0 0 0 )
( 0 0 0 )

*Main Data.Matrix> YReflection left3x3 
( 0 0 1 )
( 0 0 1 )
( 0 0 1 )
_______________________________________________________________________________

rotate90 rotates a matrix by 90 degrees

> rotate90 matrix = yReflection $ Data.Matrix.transpose matrix

*Main Data.Matrix> rotate90 top3x3 
( 0 0 1 )
( 0 0 1 )
( 0 0 1 )

*Main Data.Matrix> rotate90 $ rotate90 top3x3 
( 0 0 0 )
( 0 0 0 )
( 1 1 1 )

*Main Data.Matrix> rotate90 $ rotate90 $ rotate90 top3x3 
( 1 0 0 )
( 1 0 0 )
( 1 0 0 )
_______________________________________________________________________________

otherRotations evalueates to the 3 other rotations of the given matrix

> otherRotations matrix = [rotate90 matrix,
>			 rotate90 (rotate90 matrix),
>			 rotate90 (rotate90 (rotate90 matrix))]

*Main Data.Matrix> displayBoards $ otherRotations top3x3 
[0,0,1]    [0,0,0]    [1,0,0]
[0,0,1]    [0,0,0]    [1,0,0]
[0,0,1]    [1,1,1]    [1,0,0]
_______________________________________________________________________________

reflectionsAndRotations evalueates to all possibly unique relfections and
rotations of matrix, excepting the original. The rotations are only taken from 
one of the reflections because taking from both xReflect and yReflect results 
in duplicates.

> reflectionsAndRotations matrix =    (otherRotations matrix) 
>				      ++ [xReflection matrix] 
>				      ++ (otherRotations (xReflection matrix))

> weirdMatrix = fromLists [[1,1,0],[0,1,1],[1,0,0]]

*Main Data.Matrix> displayBoards [weirdMatrix]
[1,1,0]
[0,1,1]
[1,0,0]

*Main Data.Matrix> displayBoards $ reflectionsAndRotations weirdMatrix 
[1,0,1]    [0,0,1]    [0,1,0]    [1,0,0]    [1,0,1]    [0,1,1]    [0,1,0]
[0,1,1]    [1,1,0]    [1,1,0]    [0,1,1]    [1,1,0]    [1,1,0]    [0,1,1]
[0,1,0]    [0,1,1]    [1,0,1]    [1,1,0]    [0,1,0]    [0,0,1]    [1,0,1]
_______________________________________________________________________________

This is from stackoverflow : https://stackoverflow.com/questions/22484911/how-to-delete-a-sublist-of-a-list-in-haskell

> listDifference list = filter(`notElem` list)
> matrixList = [top3x3, left3x3]

*Main Data.Matrix> displayBoards $ equalMatrixFilter top3x3 matrixList 
[1,0,0]
[1,0,0]
[1,0,0]

*Main Data.Matrix> displayBoards $ equalMatrixFilter left3x3  matrixList 
[1,1,1]
[0,0,0]
[0,0,0]
_______________________________________________________________________________

removeHeadRAndR removes all rotations and reflections of the first element from 
the list. The first element is also ommited from the resultant list.

> removeHeadRAndR list = listDifference (reflectionsAndRotations (head list)) (tail list)

*Main Data.Matrix> displayBoards $ removeHeadRAndR $ nqueens 4
Nothing to display
(This is correct because there is only one unique solution to nqueens 4)

*Main Data.Matrix> displayBoards $ removeHeadRAndR $ nqueens 5
[0,0,0,1,0]    [0,1,0,0,0]
[1,0,0,0,0]    [0,0,0,0,1]
[0,0,1,0,0]    [0,0,1,0,0]
[0,0,0,0,1]    [1,0,0,0,0]
[0,1,0,0,0]    [0,0,0,1,0]

_______________________________________________________________________________

removeAllRAndR calls removeHeadRAndR to remove all of the rotations and 
reflections of all the boards. The unique boards are added to a list on the
recursive unwind.

> removeAllRAndR [] = []
> removeAllRAndR list = do let newList = removeHeadRAndR list
>		   	   head list:(removeAllRAndR newList)

*Main Data.Matrix> displayBoards $ removeAllRAndR $ nqueens 4
[0,0,1,0]
[1,0,0,0]
[0,0,0,1]
[0,1,0,0]

*Main Data.Matrix> displayBoards $ removeAllRAndR $ nqueens 5
[0,0,1,0,0]    [0,0,0,1,0]
[1,0,0,0,0]    [1,0,0,0,0]
[0,0,0,1,0]    [0,0,1,0,0]
[0,1,0,0,0]    [0,0,0,0,1]
[0,0,0,0,1]    [0,1,0,0,0]

*Main Data.Matrix> displayBoards $ removeAllRAndR $ nqueens 6
[0,0,0,1,0,0]
[1,0,0,0,0,0]
[0,0,0,0,1,0]
[0,1,0,0,0,0]
[0,0,0,0,0,1]
[0,0,1,0,0,0]

*Main Data.Matrix> displayBoards $ removeAllRAndR $ nqueens 7
[0,0,0,1,0,0,0]    [0,0,1,0,0,0,0]    [0,0,0,1,0,0,0]    [0,0,0,1,0,0,0]    [0,0,1,0,0,0,0]    [0,0,0,0,1,0,0]
[1,0,0,0,0,0,0]    [0,0,0,0,0,1,0]    [0,0,0,0,0,1,0]    [1,0,0,0,0,0,0]    [1,0,0,0,0,0,0]    [1,0,0,0,0,0,0]
[0,0,0,0,1,0,0]    [0,1,0,0,0,0,0]    [1,0,0,0,0,0,0]    [0,0,1,0,0,0,0]    [0,0,0,0,0,1,0]    [0,0,0,0,0,1,0]
[0,1,0,0,0,0,0]    [0,0,0,0,1,0,0]    [0,0,1,0,0,0,0]    [0,0,0,0,0,1,0]    [0,0,0,1,0,0,0]    [0,0,0,1,0,0,0]
[0,0,0,0,0,1,0]    [1,0,0,0,0,0,0]    [0,0,0,0,1,0,0]    [0,1,0,0,0,0,0]    [0,1,0,0,0,0,0]    [0,1,0,0,0,0,0]
[0,0,1,0,0,0,0]    [0,0,0,1,0,0,0]    [0,0,0,0,0,0,1]    [0,0,0,0,0,0,1]    [0,0,0,0,0,0,1]    [0,0,0,0,0,0,1]
[0,0,0,0,0,0,1]    [0,0,0,0,0,0,1]    [0,1,0,0,0,0,0]    [0,0,0,0,1,0,0]    [0,0,0,0,1,0,0]    [0,0,1,0,0,0,0]

_______________________________________________________________________________

> main = do putStrLn ""
> 	    putStrLn "Welcome to Rubin Stricklin's N-Queen solver!"
>	    putStr "Would you like to see all solutions or only unique ones? (A/U): "
>	    (x:xs) <- getLine
>	    putStr "How big would you like the board to be?: "
>	    n <- getLine
>	    if x == 'A'
>	       then displayBoards $ nqueens (read n)
>	       else displayBoards $ removeAllRAndR $ nqueens (read n)
>	    putStrLn ""
