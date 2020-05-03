data Cell = Alive | Dead deriving (Eq, Show)
type GridPoint = (Integer, Integer)
type Grid = [ [ (GridPoint, Cell) ] ]

initLine :: Integer -> Integer -> [(GridPoint, Cell)] 
initLine line maxCol = [ ((line, y), Dead) | y <- [0..maxCol] ]

initGrid :: Integer -> Integer -> Grid
initGrid maxLines maxCol = [ initLine x maxCol | x <- [0..maxLines] ]  

printCell :: Cell -> String
printCell c = if c == Alive then "x" else "."

printGridLine :: [(GridPoint, Cell)] -> String
printGridLine ((a, cell) : rest) = printCell cell ++ printGridLine rest
printGridLine _ = ""

printGrid :: Grid -> String
printGrid (line : rest) = (printGridLine line) ++ "\n" ++ (printGrid rest)
printGrid _ = ""

adjacents :: GridPoint -> [GridPoint]
adjacents (x,y) = [(x+m, y+n) | m <- [-1,0,1], n <- [-1,0,1], (m,n) /= (0,0)]

getAdjCells1 :: Grid -> GridPoint -> [Cell]
getAdjCells1 (line : rest) p =  (getAdjCells2 line (adjacents p)) ++ (getAdjCells1 rest p)
getAdjCells1 _ _ = []

getAdjCells2 :: [(GridPoint, Cell)] -> [GridPoint] -> [Cell]
getAdjCells2 ((p, cell) : rest) targetList = 
    if (elem p targetList) then [cell] ++ (getAdjCells2 rest targetList)
    else (getAdjCells2 rest targetList)
getAdjCells2 _ _ = []

nextStep :: Cell -> [Cell] -> Cell
nextStep Alive list 
    | count Alive list < 2 = Dead
    | count Alive list > 3 = Dead
    | otherwise = Alive 
nextStep Dead list
    | count Alive list == 3 = Alive
    | otherwise = Dead

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

--matrixOp :: Grid -> Grid
--matrixOp (line : rest) =  

test = initGrid 10 10

main :: IO()
main = putStrLn (printGrid test)
