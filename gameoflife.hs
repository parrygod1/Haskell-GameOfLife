module Gameoflife(
    Cell(Alive, Dead),
    GridPoint,
    Grid,
    History,
    Generation,
    initGrid,
    stringGrid,
    progressMatrix,
    parseGrid,
    getPointList,
    getBounds,
    calcAllAdjacents
) where

import Data.Map as M

data Cell = Alive | Dead deriving (Eq, Ord)
type GridPoint = (Integer, Integer)
type Grid = [ (GridPoint, Cell) ]
type History = [Grid]
type Generation = Integer

instance Show Cell where
    show Alive = "x"
    show Dead = "."

--formula for grid setup
genCell :: Integer -> Integer -> Cell
genCell a b = if (a+b) `mod` 2 == 0
    then Alive
    else Dead
--only for testing
initGrid :: Integer -> Integer -> Grid
initGrid maxLines maxCol = [ ((x, y), (genCell x y)) | x <- [0..maxLines], y <- [0..maxCol]]  
-----------------------

stringGrid :: Grid -> Integer -> Integer -> String
stringGrid (((x,y), cell) : rest) boundX boundY = 
    if y == boundY then show cell ++ "\n" ++ (stringGrid rest boundX boundY)
    else show cell ++ (stringGrid rest boundX boundY)
stringGrid _ _ _= ""

--finds the indexes from a point's neighbors
calcAdjacents :: GridPoint -> M.Map GridPoint Int -> [Int]
calcAdjacents (l,c) map = [ (map M.! (l+m, c+n)) | m <- [-1,0,1], n <- [-1,0,1], (member (l+m,c+n) map) && (m,n) /= (0,0)]

--creates a list with a gridpoint and its neighbors indexes to be used in a map
calcAllAdjacents :: Grid -> M.Map GridPoint Int -> [(GridPoint, [Int])]
calcAllAdjacents ((point, cell) : rest) map = [(point, adjIndexes)] ++ calcAllAdjacents rest map
    where adjIndexes = (calcAdjacents point map)
calcAllAdjacents _ _ = []

getCellFromIndex :: Grid -> Int -> Cell
getCellFromIndex grid index = snd $ grid !! index

buildAdjCells :: Grid -> [Int] -> [Cell]
buildAdjCells grid (index : rest) = [(getCellFromIndex grid index)] ++ buildAdjCells grid rest
buildAdjCells _ _ = []

getAdjCells :: GridPoint -> Grid -> M.Map GridPoint [Int] -> [Cell]
getAdjCells point grid adjMap = buildAdjCells grid $ adjMap M.! point

nextStep :: Cell -> [Cell] -> Cell
nextStep Alive list 
    | count Alive list < 2 = Dead
    | count Alive list > 3 = Dead
    | otherwise = Alive 
nextStep Dead list
    | count Alive list == 3 = Alive
    | otherwise = Dead

count :: Eq a => a -> [a] -> Int
count x = length . Prelude.filter (== x)

--first arg is remaining list, second the whole grid
progressMatrix :: [ (GridPoint, Cell) ] -> Grid -> M.Map GridPoint [Int] -> Grid
progressMatrix ((point, cell) : rest) grid map = [newCell] ++ progressMatrix rest grid map
    where newCell = (point, (nextStep cell (getAdjCells point grid map)))
progressMatrix _ _ _ = []

parseGrid :: String -> Integer -> Integer -> Grid
parseGrid (char : rest) x y 
    | char == head (show Alive) = [((x,y), Alive)] ++ parseGrid rest x (y+1)
    | char == head (show Dead) = [((x,y), Dead)] ++ parseGrid rest x (y+1)
    | char == '\n' = parseGrid rest (x+1) 0
    | otherwise = parseGrid rest x y
parseGrid _ _ _ = []

getPointList :: Grid -> Int -> [(GridPoint, Int)]
getPointList ((point, cell) : rest) index = [(point, index)] ++ (getPointList rest (index+1))
getPointList _ _ = []

getBounds :: Grid -> (Integer, Integer)
getBounds grid = fst $ last grid 

readGrid :: Integer -> Integer -> IO ()
readGrid boundX boundY = do 
    putStrLn "Enter input file: "
    file <- getLine
    contents <- readFile file 
    --let newGrid = (parseGrid contents 0 0)
    --let map1 = M.fromList (getPointList newGrid 0)
    --let map2 = M.fromList (calcAllAdjacents newGrid map1)
    putStrLn (stringGrid (parseGrid contents 0 0) boundX boundY)

