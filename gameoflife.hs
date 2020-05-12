module Gameoflife (
    Cell(Alive, Dead),
    Grid,
    History,
    Generation,
    initGrid,
    stringGrid,
    progressMatrix
) where

data Cell = Alive | Dead deriving (Eq)
type GridPoint = (Integer, Integer)
type Grid = [ (GridPoint, Cell) ]
type History = [Grid]
type Generation = Integer

instance Show Cell where
    show Alive = "x"
    show Dead = "."

--TODO: use random
getCell :: Integer -> Integer -> Cell
getCell a b = if (a+b) `mod` 2 == 0
    then Alive
    else Dead

initGrid :: Integer -> Integer -> Grid
initGrid maxLines maxCol = [ ((x, y), (getCell x y)) | x <- [0..maxLines], y <- [0..maxCol]]  

stringGrid :: Grid -> Integer -> Integer -> String
stringGrid (((x,y), cell) : rest) boundX boundY = 
    if y == boundY then show cell ++ "\n" ++ (stringGrid rest boundX boundY)
    else show cell ++ (stringGrid rest boundX boundY)
stringGrid _ _ _= ""

--TODO: make data for remembering adjacent cells so this doesn't get called every step
adjacents :: GridPoint -> [GridPoint]
adjacents (x,y) = [(x+m, y+n) | m <- [-1,0,1], n <- [-1,0,1], (m,n) /= (0,0)]

getAdjCells :: [GridPoint] -> Grid -> [Cell]
getAdjCells targets ((point, cell) : rest) = 
    if elem point targets then [cell] ++ getAdjCells targets rest
    else getAdjCells targets rest
getAdjCells _ _ = []

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

--first arg is remaining list, second the whole grid
progressMatrix :: [ (GridPoint, Cell) ] -> Grid -> Grid
progressMatrix ((p, c) : rest) g = [ (p, (nextStep c (getAdjCells (adjacents p) g))) ] ++ progressMatrix rest g
progressMatrix _ _= []